;; -----------------------------------------------------------------------------
;; Filename : setup-smartparens.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : setup file for smartparens.
;; -----------------------------------------------------------------------------

(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)

;; -----------------------------------------------------------------------------
;;     Smartparens with modes
;; -----------------------------------------------------------------------------

;; Latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-pair "$" "$"))

;; C-mode
;; (sp-with-modes '(c-mode c++-mode)
;;   (sp-local-pair "<" ">"))


(provide 'setup-smartparens)
