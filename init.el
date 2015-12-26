;; Pierre-Antoine ROBERT <pierreantoine dot robert at gmail dot com>

(setq user-full-name "Pierre-Antoine ROBERT"
      user-mail-adress "pierreantoine.robert@gmail.com"
      query-user-mail-adress nil)

;; Mettre en plein écran
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Indication des répertoirs
(let ((default-directory  "~/.emacs.d/site-lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory  "~/.tmp/emacs"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory  "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


;; Chargement de mes fichiers de config
(load "init-require")
(load "init-theme")
(load "init-functions")
(load "init-keys")

;; Packages
(eval-after-load "package"
  '(progn
     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
     (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
     (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)     
     (package-initialize)))
(setq package-user-dir "~/.emacs.d/site-lisp")

;; Empêche l'outil de personnalisation d'emacs de toucher à ce fichier
(setq custom-file "~/.tmp/emacs/custom/emacs-custom.el")
(load custom-file) 

;; Mode par défault
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(setq major-mode 'text-mode)

;; Choix du mode selon l'extension du fichier
(setq auto-mode-alist
      '(
	("\\.\\(el\\|emacs\\)$" . emacs-lisp-mode)
        ("\\.tex$" . LaTeX-mode)
        ("\\.bib$" . bibtex-mode)
        ("\\.py$" . python-mode)
        ("\\.\\(r\\|R\\)$" . R-mode)
        ("\\.\\(c\\|h\\|cpp\\)$" . c++-mode)
        ("\\.lua" . lua-mode)
        ("\\.\\(md\\|markdown\\)" . markdown-mode)))

(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
(add-hook 'LaTeX-mode-hook 'font-lock-mode)
(add-hook 'bibtex-mode-hook 'font-lock-mode)
(add-hook 'R-mode-hook 'font-lock-mode)
(add-hook 'python-mode 'font-lock-mode)

;; Empêcher les informations inutiles en quitant emacs
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

;; Stocker les fichiers~ dans un dossier et les supprimer régulièrement
(setq make-backup-files t
      backup-by-copying t
      backup-dir "~/.tmp/emacs/backup/"
      auto-save-file-name-transforms `((".*" ,"~/.tmp/emacs/backup/" t))
      tramp-auto-save-directory "~/.tmp/emacs/backup/"
      delete-old-versions t)
(add-to-list 'backup-directory-alist
             `(".*" . ,backup-dir))
(suppression-automatique-demarrage)

;; Ecrire y ou n à la place de yes ou no
(fset 'yes-or-no-p 'y-or-n-p)

;; Iteractively do things
(ido-mode t)

;; ---- Company-mode ----

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook (lambda () (company-quickhelp-mode t)))
(add-to-list 'company-backends 'company-math-symbols-unicode 'company-c-headers)

(setq
 company-tooltip-limit 20
 company-idle-delay 0.2
 company-echo-delay 0
 company-show-numbers t
 company-minimum-prefix-length 2
 company-statistics-file "~/.tmp/emacs/company/company-statistics-cache.el")

;; ---- Ispell ----

(setq
 ispell-program-name "aspell"
 ispell-dictionary "francais")

;; ---- Python-mode ----

(add-hook
 'python-mode-hook
 (lambda ()
   (add-to-list 'company-backends 'company-jedi)
   '(fci-mode t)))

;; Complétion par Company
(set (make-local-variable 'py-electric-close-active-p) t)
(set (make-local-variable 'py-auto-complete-p) nil)
(set (make-local-variable 'py-auto-completion-mode-p) nil)
(set (make-local-variable 'py-company-pycomplete-p) t)
(set (make-local-variable 'py-complete-auto) 'py-complete)
(set (make-local-variable 'py-complete-function) 'nil)
;; (py-timer-close-completions-p )
;; (py-complete-ac-sources )
;; (py-shell-module-completion-code "")
;; (py-ipython-module-completion-string "")

;; Retour à la ligne automatque
(set (make-local-variable 'py-auto-fill-mode) t)
(set (make-local-variable 'py-comment-fill-column) 80)
(set (make-local-variable 'py-docstring-fill-column) 80)

;; Mise en forme du code python dans un interpréteur shell
(set (make-local-variable 'py-fontify-shell-buffer-p) t)

;; Auto-enregistrement avant interprétation
(set (make-local-variable 'py-ask-about-save) nil)

;; Mise en forme du docstring
(set (make-local-variable 'py-docstring-style) 'django)

;; ---- Lua ----

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-indent-level 4)


;; ---- ESS-mode ----

;; (add-hook 'ess-mode-hook '(autoload 'R-mode "ess-site.el" "" t))
(autoload 'R-mode "ess-site.el" "ESS" t)
(add-hook 'ess-mode-hook (lambda () (company-statistics-mode t)))

(setq ess-ask-for-ess-directory nil)
(setq comint-input-ring-size 1000)
(setq ess-indent-level 4)
(setq ess-arg-function-offset 4)
(setq ess-else-offset 4)

;; ---- AucTex ----

(load "auctex.el" nil t t)
(load "font-latex")

(add-hook
 'LaTeX-mode-hook
 (lambda ()
   'flyspell-mode
   'flyspell-buffer
   'LaTeX-math-mode
   'turn-on-auto-fill
   'fill-start
   'my-latex-mode-setup))

(tex-font-setup)
(setq
 Tex-auto-save t
 Tex-parse-self t
 Tex-save-query nil)

;; (setq TeX-electric-escape t)
(company-auctex-init)

;; ---- Text-mode ----

;; (load "init-latex")
(add-hook 'text-mode-hook
          (lambda ()
            'flyspell-mode
            'turn-on-auto-fill
            'fill-start))

;; ---- C-mode ----

(setq-default
 c-basic-offset 4
 c-default-style "linux")


;; ---- Eshell ----

(setq eshell-history-file-name "~/.tmp/emacs/eshell/history"
      eshell-last-dir-ring-file-name "~/.tmp/emacs/eshell/lastdir") 

;; ---- Scilab ----

(load "scilab-startup")


;; ---- END ----
(kill-buffer "*Compile-Log*")
