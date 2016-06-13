;; --------------------------------------------------------------------------------
;; Filename : init-terminal-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for terminal mode. It is the same as the shell mode, but for a regular terminal (not the shell one).
;; --------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun exist-terminal-p ()
  "Gives t if buffer *terminal* exist and nil if not."
  (interactive)
  (buffer-live-p (get-buffer "*terminal*")))

(defun open-terminal ()
  "Open a terminal window."
  (interactive)
  (split-auto)
  (term "bash")
  (term-mode)
  (term-char-mode)
  (global-set-key (kbd "C-c C-t") 'close-terminal))

(defun go-to-terminal ()
  "Go to the terminal window."
  (interactive)
  (setq window (get-buffer-window "*terminal*"))
  (select-window window))

(defun windnew-terminal ()
  "Look for an existing terminal window and open one if it does not exist yet."
  (interactive)
  (if (exist-terminal-p)
      (go-to-terminal)
    (open-terminal)))

(defun close-terminal ()
  "Shut down the terminal window."
  (interactive)
  (term-send-string "*terminal*" "exit")
  (kill-buffer-and-window)
  (global-set-key (kbd "C-c C-t") 'open-terminal))

(defadvice close-terminal (around stfu compile activate)
  "No question for shutting down the shell window."
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

;; --------------------------------------------------------------------------------
;;     Key map
;; --------------------------------------------------------------------------------

(global-set-key (kbd "C-c C-t") 'windnew-terminal)


(provide 'init-terminal-mode)
