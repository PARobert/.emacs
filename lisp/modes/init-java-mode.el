;; --------------------------------------------------------------------------------
;; Filename : init-java-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for java mode.
;; --------------------------------------------------------------------------------

(require 'cc-mode)

(setq-default c-basic-offset 4)
(setq-default c-default-style '((java-mode . "java")))
(setq-default c-indent-tabs-mode t)


;; --------------------------------------------------------------------------------

(provide 'init-java-mode)
