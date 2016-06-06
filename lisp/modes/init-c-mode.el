;; --------------------------------------------------------------------------------
;; Filename : init-c-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for C/C++ mode.
;; --------------------------------------------------------------------------------

(require 'cc-mode)

(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")

;; --------------------------------------------------------------------------------
;;     Mode map
;; --------------------------------------------------------------------------------

(defun mon-c++-mode-map ()
  "Définition des raccourcis clavier du C++-mode"
  (local-set-key (kbd "C-c C-c") 'mon-c-compilation)
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'c++-mode-hook 'mon-c++-mode-map)


(provide 'init-c-mode)
