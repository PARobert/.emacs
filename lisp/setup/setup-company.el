;; -----------------------------------------------------------------------------
;; Filename : setup-company.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : Setup file for company.
;; -----------------------------------------------------------------------------

(require 'company)
(require 'company-auctex)
(require 'company-math)
(require 'pos-tip)
(require 'company-quickhelp)
;; (require 'company-statistics)
(require 'company-jedi)
(require 'company-ess)

(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode t)

(add-to-list 'company-backends 'company-ess-backend)
(add-to-list 'company-backends 'company-math-symbols-unicode)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-auctex-labels)
(add-to-list 'company-backends 'company-auctex-bibs)
(add-to-list 'company-backends 'company-auctex-macros)
(add-to-list 'company-backends 'company-auctex-symbols)
(add-to-list 'company-backends 'company-auctex-environments)
(add-to-list 'company-backends 'company-anaconda)

(setq company-begin-commands '(self-insert-command))

(setq company-tooltip-limit 20)
(setq company-idle-delay 0.05)
(setq company-echo-delay 0)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
;; (setq company-statistics-file "~/.tmp/emacs/company/company-statistics-cache.el")

;; -----------------------------------------------------------------------------
;;     Functions
;; -----------------------------------------------------------------------------

(defun exist-company-help-p ()
  "Renvoie t si le buffer *Help* existe et nil sinon"
  (interactive)
  (buffer-live-p (get-buffer "*Help*")))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(defun company-quick-description ()
  "Ouvre la description des fonctions dans une nouvelle fenêtre"
  (interactive)
  (if (exist-company-help-p)
      (describe-function (function-called-at-point))
    (save-excursion
      (split-auto)
      (switch-to-buffer "*Help*"))
      (help-mode))
    (describe-function (function-called-at-point)))

(defun my/python-company ()
  (add-to-list 'company-backends 'company-jedi))



;; (defun company-my-backend (command &optional arg &rest ignored)
;;   (pcase command
;;     (`prefix (company-grab-symbol))
;;     (`candidates (list "foobar" "foobaz" "foobarbaz"))
;;     (`meta (format "This value is named %s" arg))))


;; -----------------------------------------------------------------------------
;;     Hooks
;; -----------------------------------------------------------------------------

;; (add-hook 'python-mode-hook #'my/python-company)

;; -----------------------------------------------------------------------------
;;     Fonts
;; -----------------------------------------------------------------------------

(set-face-background 'company-tooltip my-err)
(set-face-background 'company-tooltip-selection my-bg-comp)
(set-face-background 'company-tooltip-common my-err)
(set-face-background 'company-tooltip-common-selection my-bg-comp)
(set-face-foreground 'company-tooltip "White")
(set-face-foreground 'company-tooltip-selection "White")
(set-face-foreground 'company-tooltip-common my-fg)
(set-face-foreground 'company-tooltip-common-selection my-fg)
(set-face-background 'company-scrollbar-fg "White")
(set-face-background 'company-scrollbar-bg my-bg)

;; -----------------------------------------------------------------------------
;;     Mode-map
;; -----------------------------------------------------------------------------

(global-set-key (kbd "\t") 'company-complete-common)
(global-set-key (kbd "C-c h") 'company-quick-description)



;; -----------------------------------------------------------------------------

(provide 'setup-company)
