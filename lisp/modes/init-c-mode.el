;; -----------------------------------------------------------------------------
;; Filename : init-c-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for C/C++ mode.
;; -----------------------------------------------------------------------------

(require 'cc-mode)

(setq-default c-basic-offset 4)
(setq-default c-default-style '((c-mode . "linux")))
(setq-default c-indent-tabs-mode t)

;; -----------------------------------------------------------------------------
;;     Variables
;; -----------------------------------------------------------------------------

(defvar my-c-source)

;; -----------------------------------------------------------------------------
;;     Functions
;; -----------------------------------------------------------------------------

(defun my/c-compile ()
  "C/C++ compilation with make."
  (interactive)
  (save-buffer)
  (compile "make"))

(defun my/gdb-display-gdb ()
  "Display the gdb buffer."
  (interactive)
  (switch-to-buffer gud-comint-buffer))

(defun my/gdb-display-io ()
  "Display the io buffer."
  (interactive)
  (switch-to-buffer (gdb-get-buffer-create 'gdb-inferior-io)))

(defun my/c-debugg-map ()   
  (gud-def my-run "run" "\C-s" "Run the debugger."))

(defun my/c-debugg ()
  "Start gdb."
  (interactive)
  (setq my-c-source (window-buffer))
  (delete-other-windows)
  (gdb (gud-query-cmdline 'gdb))
  (my/c-debugg-map)
  (switch-to-buffer my-c-source)
  (delete-other-windows)
  (split-droite)
  (save-selected-window
    (my/gdb-display-gdb)
    (split-bas)
    (my/gdb-display-io)))

;; -----------------------------------------------------------------------------
;;     Mode map
;; -----------------------------------------------------------------------------

(defun my/c-c++-mode-map ()
  "Short cuts for C++ and C modes."
  (local-set-key (kbd "C-c C-c") 'my/c-debugg)
  (local-set-key (kbd "<C-return>") 'my/c-compile)
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'c-mode-hook 'my/c-c++-mode-map)
(add-hook 'c++-mode-hook 'my/c-c++-mode-map)

;; -----------------------------------------------------------------------------

(provide 'init-c-mode)
