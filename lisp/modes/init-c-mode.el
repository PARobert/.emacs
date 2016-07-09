;; --------------------------------------------------------------------------------
;; Filename : init-c-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for C/C++ mode.
;; --------------------------------------------------------------------------------

(require 'cc-mode)

(setq-default c-basic-offset 4)
(setq-default c-default-style '((c-mode . "linux")))
(setq-default c-indent-tabs-mode t)

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun my/c-compile ()
  "C/C++ compilation with make."
  (interactive)
  (save-buffer)
  (compile "make"))

;; --------------------------------------------------------------------------------
;;     Mode map
;; --------------------------------------------------------------------------------

(defun my/c-c++-mode-map ()
  "Short cuts for C++ and C modes."
  (local-set-key (kbd "C-c C-c") 'mon-c-compilation)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (local-set-key (kbd "<C-return>") 'my/c-compile))

(add-hook 'c-mode-hook 'my/c-c++-mode-map)
(add-hook 'c++-mode-hook 'my/c-c++-mode-map)


;; --------------------------------------------------------------------------------

(provide 'init-c-mode)
