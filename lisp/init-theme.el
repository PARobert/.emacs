;; Thème d'affichage
(add-to-list 'load-path 
             "~/.emacs.d/color-theme"
             "~/.emacs.d/color-theme/themes")
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (setq color-theme-is-global t)
;;      (color-theme-billw)))
;; (color-theme-robin-hood)

;; Enlever l'affichage de l'écran d'acceuil au démarrage d'emacs.
(setq inhibit-startup-screen 1)

;; Supprimer les bares de menu
(menu-bar-mode -1)
(tool-bar-mode -1) 
(scroll-bar-mode -1)
(mouse-wheel-mode -1)

;; Desactive les bips d'emacs
(setq visible-bell t)

;; Numerotation des lignes
(add-hook 'find-file-hook
          (lambda ()
            (linum-mode 1)))
(setq line-number-mode nil)
(setq column-number-mode nil)

;; Visualise les parenthèses, crochets et accolades.
(eval-after-load "paren"
  '(show-paren-mode t))

;; Ajout automatique des crochets, parenthèses, guillemets, etc. fermants.
(electric-pair-mode 1)

;; Indiquer par une barre verticale la limite de la ligne
;; (define-globalized-minor-mode
;;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)
;; (setq fci-rule-color "#626262")

;; Nom du buffer dans le titre
(setq frame-title-format "%b - Emacs")
(setq icon-title-format "%b - Emacs")

;; Les tabulations sont 4 espaces
(setq basic-offset 4)
(setq-default indent-tabs-mode nil)

;; Toujours s'assurer que le fichier termine par une ligne vide
(setq require-final-newline 't)

;; Nombres de lignes à sauter
(setq-default scroll-conservatively 5)
(setq-default scroll-step 1)
 
;; Retours à la ligne automatique à 80 carractères
(setq-default fill-column 80)
(setq fill-nobreak-predicate 'my-fill-nobreak-predicate)

;; Écrase la sélection lorsqu'on écrit par dessus
(delete-selection-mode 1)

;; Respect des normes typographiques françaises.
(setq fill-nobreak-predicate '(fill-french-nobreak-p))
(setq sentence-end-double-space nil)

;; Colors
(defvar my-bg "Grey24")
(defvar my-fg "SkyBlue1")
(defvar my-err "Grey26")
(defvar my-ppl "Azure1")
(defvar my-fond "Grey16")
(defvar my-bg-comp "Grey22")

;; Emacs base faces
(set-face-background 'isearch my-fg)
(set-face-background 'isearch-fail "SteelBlue3")
(set-face-background 'lazy-highlight "SkyBlue3")
(set-face-background 'mode-line my-bg)
(set-face-background 'mode-line-inactive my-bg)
(set-face-background 'query-replace my-fg)
(set-face-background 'region my-bg)
(set-face-background 'vertical-border nil)

(set-face-bold-p 'font-lock-function-name-face 1)
(set-face-bold-p 'font-lock-keyword-face 1)
(set-face-bold-p 'font-lock-type-face 1)
(set-face-bold-p 'minibuffer-prompt 1)

(set-face-foreground 'font-lock-builtin-face "LightSkyBlue1")
(set-face-foreground 'font-lock-comment-face "IndianRed3")
(set-face-foreground 'font-lock-constant-face "Magenta")
(set-face-foreground 'font-lock-function-name-face "SteelBlue3")
(set-face-foreground 'font-lock-keyword-face "LightSkyBlue1")
(set-face-foreground 'font-lock-preprocessor-face "Magenta")
(set-face-foreground 'font-lock-string-face "AquaMarine3")
(set-face-foreground 'font-lock-type-face "AquaMarine3")
(set-face-foreground 'font-lock-variable-name-face nil)
(set-face-foreground 'isearch nil)
(set-face-foreground 'isearch-fail nil)
(set-face-foreground 'minibuffer-prompt my-fg)
(set-face-foreground 'mode-line my-fg)
(set-face-foreground 'mode-line-inactive my-fg)
(set-face-foreground 'region my-fg)
(set-face-foreground 'vertical-border my-bg)

(set-face-background 'default my-fond)
(set-face-foreground 'default "White")

(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line nil :box nil)
(set-face-foreground 'vertical-border my-bg)
(set-face-background 'fringe my-fond)
(set-face-foreground 'fringe my-fg)
(set-face-foreground 'shadow "Dim gray") ; Défault : Grey70

;; ---- Company-mode ----

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

