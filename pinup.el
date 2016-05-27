(require 'cl)

(defgroup pinup nil
  "Pin and manage pinned windows."
  :group 'tools
  :group 'convenience)

(defun pinup-window ()
  "Toggle whether the current window is pinned or not.

            Pinned windows are not killed by `pinup-kill-other-windows' and they are dedicated.
            See `set-window-dedicated-p' about dedicated windows."
  (interactive)
  (message
   (if (let (window (pinup--get-current-buffer-window))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
       "Pinning '%s' window"
     "Unpinning '%s' window")
   (current-buffer)))

(defun pinup-kill-other-windows ()
  "Kill non `current-buffer' windows that have not been pinned with `pinup-window'."
  (interactive)
  (loop for window in (pinup--list-visible-windows)
	do (unless (window-dedicated-p window)
	     (unless (eq (pinup--get-current-buffer-window) window)
	       (delete-window window)))))

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

(defcustom pinup-keymap-prefix (kbd "C-x p")
  "Pinup keymap prefix."
  :group 'pinup
  :type 'string)

(defvar pinup-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'pinup-window)
    (define-key map (kbd "1") #'pinup-kill-other-windows)
    map)
  "Keymap for Pinup commands after `pinup-keymap-prefix'.")
