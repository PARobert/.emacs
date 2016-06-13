;; --------------------------------------------------------------------------------
;; Filename : init-shell-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for emuling a shell in emacs.
;; See http://www.nongnu.org/emacsdoc-fr/manuel/shell.html for more details and
;; options about shell-mode, commands and options.
;; --------------------------------------------------------------------------------


;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun exist-shell-p ()
  "Gives t if buffer *shell* exist and nil if not."
  (interactive)
  (buffer-live-p (get-buffer "*shell*")))

(defun open-shell ()
  "Open a shell in a new window."
  (interactive)
  (split-auto)
  (shell)
  (global-set-key (kbd "C-c C-t") 'close-shell))

(defun go-to-shell ()
  "Go to the shell window."
  (interactive)
  (setq window (get-buffer-window "*shell*"))
  (select-window window))

(defun windnew-shell ()
  "Look for an existing shell window and open one if it does not exist yet."
  (interactive)
  (if (exist-shell-p)
      (go-to-shell)
    (open-shell)))

(defun close-shell ()
  "Shut down the shell window."
  (interactive)
  (shell-command "exit")
  (kill-buffer-and-window)
  (kill-buffer "*Shell Command Output*")
  (global-set-key (kbd "C-c C-t") 'open-shell))

(defadvice close-shell (around stfu compile activate)
  "No question for shutting down the shell window."
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

;; --------------------------------------------------------------------------------
;;     Key map
;; --------------------------------------------------------------------------------

(global-set-key (kbd "C-c C-t") 'windnew-shell)


(provide 'init-shell-mode)
