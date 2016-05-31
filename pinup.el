;;; pinup.el --- Manage dedicated windows -*- logical-binding: t -*-

;; Copyright © 2016 Johnson Denen <johnson.denen@gmail.com>

;; Author: Johnson Denen <johnson.denen@gmail.com>
;; URL: https://github.com/jdenen/pinup
;; Keywords: dedicated, pinned, convenience
;; Version: 0.14.0-cvs

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

;; Pinup provides functions to support a dedicated, ever-present window
;; for visibility and easy access.  For a full description and examples
;; of its use, see the `README.md' file packaged with pinup.el.

;; This library provides easy management of dedicated windows.  A
;; dedicated window is one which will not display another buffer in
;; it.  This library refers to dedicated windows as "pinned." See
;; README for more details.
;;
;;; Code:

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

(defvar pinup-pinned-window nil
  "Currently pinned window.")

(defvar pinup-pinned-window-normal-width nil
  "Pinned window width before minification.")

(defun pinup-pin ()
  "Pin current window.

If `pinup-mode' is enabled, this function will update
the mode line with 'Pinned'.

Pinnings write over their predecessors, so only one
window may be pinned at a time."
  (interactive)
  (pinup--set-pinned-window (pinup--get-current-buffer-window))
  (pinup--update-mode-line " Pinned"))

(defun pinup-unpin ()
  "Unpin the pinned window.

If `pinup-mode' is enabled, this function will remove
'Pinned' from the mode line."
  (interactive)
  (pinup--set-pinned-window nil)
  (pinup--update-mode-line ""))

(defun pinup-delete-other-windows ()
  "Delete other unpinned windows."
  (interactive)
  (dolist (win (window-list) nil)
    (unless (or (eq pinup-pinned-window win)
		(eq (pinup--get-current-buffer-window) win)
      (delete-window win)))))

(defun pinup-minimize ()
  "Minimize the pinned window."
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
  "Set `pinup-pinned-window' to WINDOW."
  (if window
      (setq pinup-pinned-window window)
    (setq pinup-pinned-window nil)))

(defun pinup--get-pinned-window-width ()
  "Get width of the currently pinned window."
  (window-width pinup-pinned-window))

(defun pinup--get-current-buffer-window ()
  "Return window for the `current-buffer'."
  (get-buffer-window (current-buffer)))

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
  "Minor mode to assist with window management."
  :lighter pinup-mode-line
  :group 'pinup
  :require 'pinup)

(provide 'pinup-mode)
;;; pinup.el ends here
