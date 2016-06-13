;; --------------------------------------------------------------------------------
;; Filename : init-text-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for text mode.
;; --------------------------------------------------------------------------------

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'fill-start)

