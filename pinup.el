;;; pinup.el --- Manage dedicated windows -*- logical-binding: t -*-

;; Copyright © 2016 Johnson Denen <johnson.denen@gmail.com>

;; Author: Johnson Denen <johnson.denen@gmail.com>
;; URL: https://github.com/jdenen/pinup
;; Keywords: dedicated, pinned, convenience
;; Version: 0.14.0-cvs
;; Package-Requires: ((dash "2.11.0") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library provides easy management of dedicated windows. A
;; dedicated window is one which will not display another buffer in
;; it. This library refers to dedicated windows as "pinned." See
;; README for more details.
;;
;;; Code:

(require 'cl)

(defgroup pinup nil
  "Pin and manage pinned windows."
  :group 'tools
  :group 'convenience)

(defcustom pinup-mode-line
  `(:eval (if (window-dedicated-p)
	      " Pinned"
	    " Unpinned"))
  "Mode line for Pinup."
  :group 'pinup
  :type 'sexp
  :risky t)

(defun pinup-toggle-window ()
  "Toggle whether the current window is pinned or not.

Pinned windows are not killed by `pinup-delete-other-windows' and they are dedicated.
See `set-window-dedicated-p' about dedicated windows."
  (interactive)
  (message
   (if (let (window (pinup--get-current-buffer-window))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
	 (progn
	   (pinup--set-pinned-window (pinup--get-current-buffer-window))
	   (pinup--update-mode-line " Pinned")
	   "Pinning '%s' window")
       (progn
	 (pinup--set-pinned-window nil)
	 (pinup--update-mode-line " Unpinned")
	 "Unpinning '%s' window"))
   (current-buffer)))

(defun pinup-delete-other-windows ()
  "Kill non `current-buffer' windows that have not been pinned with `pinup-toggle-window'."
  (interactive)
  (loop for window in (pinup--list-visible-windows)
	do (unless (window-dedicated-p window)
	     (unless (eq (pinup--get-current-buffer-window) window)
	       (delete-window window)))))

(defun pinup-minimize-pinned ()
  "Minimize the currently pinned window."
  (interactive)
  (setq pinup-pinned-window-normal-width (pinup--get-pinned-window-width))
  (if pinup-pinned-window
      (minimize-window pinup-pinned-window)
    nil))

(defun pinup-restore-pinned ()
  "Restore pinned window to width before `pinup-minimize-pinned' was called."
  (interactive)
  (window-resize pinup-pinned-window (pinup--get-pinned-width-delta) t))

(defun pinup--get-pinned-width-delta ()
  "Difference of pinned window's pre-minified width and its current width."
  (- pinup-pinned-window-normal-width (pinup--get-pinned-window-width)))

(defun pinup--set-pinned-window (&optional window)
  "Set or clear value of `pinup-pinned-window'."
  (if window
      (setq pinup-pinned-window window)
    (setq pinup-pinned-window nil)))

(defun pinup--get-pinned-window-width ()
  "Get width of the currently pinned window."
  (window-width pinup-pinned-window))

(defun pinup--get-current-buffer-window ()
  "Return window for the `current-buffer'."
  (get-buffer-window (current-buffer)))

(defun pinup--list-visible-windows ()
  "Return list of windows."
  (apply #'append (remove nil
			  (mapcar
			   '(lambda (buf)
			      (get-buffer-window-list buf))
			   (buffer-list)))))

(defun pinup--update-mode-line (text)
  "Reformat mode line with TEXT."
  (message nil)
  (with-current-buffer (current-buffer)
    (make-local-variable 'mode-line-format)
    (let ((mode-line-format text))
      (force-mode-line-update)))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode pinup-mode
  "Minor mode to assist with window management.

\\{pinup-mode-map}"
  :lighter pinup-mode-line
  :group 'pinup
  :require 'pinup)

(provide 'pinup-mode)
