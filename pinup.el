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
  `(:eval (if (eq pinup-pinned-window (pinup--get-current-buffer-window))
	      " Pinned"
	    ""))
  "Mode line for Pinup."
  :group 'pinup
  :type 'sexp
  :risky t)

(defcustom pinup-keymap-prefix (kbd "C-c u")
  "Pinup keymap prefix."
  :group 'pinup
  :type 'string)

(defcustom pinup-pinned-default-width nil
  "Constant width of pinned window."
  :group 'pinup
  :type 'integer)

(defcustom pinup-default-pinned-buffer nil
  "Default pinned window to this buffer."
  :group 'pinup
  :type 'function)

(defcustom pinup-minimize-on-other-window nil
  "Boolean to determine if pinned window is minimized on `pinup-other-window'."
  :group 'pinup
  :type 'boolean)

(defvar pinup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map pinup-keymap-prefix
      (let ((commands (make-sparse-keymap)))
	(define-key commands (kbd "u") #'pinup)
	(define-key commands (kbd "d") #'pindown)
	(define-key commands (kbd "m") #'pinup-minimize)
	(define-key commands (kbd "r") #'pinup-restore)
	(define-key commands (kbd "j") #'pinup-jump)
	(define-key commands (kbd "o") #'pinup-other-window)
	(define-key commands (kbd "1") #'pinup-delete-other-windows)
	(define-key commands (kbd "k") #'pinup-clear-pinned-window)
	commands))
    map)
  "Keymap for Pinup.")

(defvar pinup-pinned-window nil)
(defvar pinup-pinned-window-normal-width nil)

(defun pinup ()
  "Pin current window.

If `pinup-mode' is enabled, this function will update
the mode line with 'Pinned'.

Pinnings write over their predecessors, so only one
window may be pinned at a time."
  (interactive)
  (pinup--set-pinned-window (pinup--get-current-buffer-window))
  (pinup--update-mode-line " Pinned"))

(defun pindown ()
  "Unpin the pinned window.

If `pinup-mode' is enabled, this function will remove
'Pinned' from the mode line."
  (interactive)
  (if pinup-pinned-window
      (progn
	(set-window-dedicated-p pinup-pinned-window nil)
	(pinup--set-pinned-window nil)
	(pinup--update-mode-line ""))))

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

(defun pinup-restore ()
  "Restore pinned window to width before `pinup-minimize-pinned' was called."
  (interactive)
  (window-resize pinup-pinned-window (pinup--get-pinned-width-delta) t))

(defun pinup-jump ()
  "Jump to pinned window.

If the pinned window is minimized, restore it to normal width before switching."
  (interactive)
  (if pinup-pinned-window
      (progn
	(pinup-restore)
	(select-window pinup-pinned-window))
    (if pinup-default-pinned-buffer
	(progn
	  (funcall pinup-default-pinned-buffer)
	  (pinup)
	  (pinup-restore)))))

(defun pinup-other-window ()
  "Jump away from the pinned window.

If `pinup-minimize-on-other-window' is non-nil, minimize the pinned window."
  (interactive)
  (other-window 1)
  (if pinup-minimize-on-other-window
      (pinup-minimize)))

(defun pinup-clear-pinned-window ()
  "Set `pinup-pinned-window' to nil."
  (interactive)
  (setq pinup-pinned-window nil))

(defun pinup--get-pinned-width-delta ()
  "Difference of pinned window's pre-minified width and its current width."
  (- (or pinup-pinned-default-width
	 pinup-pinned-window-normal-width)
     (pinup--get-pinned-window-width)))

(defun pinup--set-pinned-window (&optional window)
  "Set `pinup-pinned-window' to WINDOW."
  (if window
      (progn
	(set-window-dedicated-p window t)
	(setq pinup-pinned-window window))
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
  "Minor mode to manage a persistant, buffer-dedicated window."
  :lighter pinup-mode-line
  :group 'pinup
  :require 'pinup
  :keymap pinup-mode-map
  :global t)

(provide 'pinup)
;;; pinup.el ends here
