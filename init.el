;; --------------------------------------------------------------------------------
;; Filename : init.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for emacs.
;; --------------------------------------------------------------------------------

(setq user-full-name "Pierre-Antoine ROBERT"
      user-mail-adress "pierre.antoine.ROBERT@ensae-paristech.fr"
      query-user-mail-adress nil)

;; Put in full screen
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
(require 'setup-packages)
(require 'setup-require)
(require 'setup-theme)
(require 'setup-functions)
(require 'setup-keys)

(require 'setup-company)
(require 'setup-header)

(require 'init-emacs-mode)
(require 'init-latex-mode)
(require 'init-ess-mode)
(require 'init-python-mode)
(require 'init-c-mode)
(require 'init-shell-mode)

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
        ("\\.\\(tex\\|lytex\\)$" . LaTeX-mode)
        ("\\.bib$" . bibtex-mode)
        ("\\.py$" . python-mode)
        ("\\.\\(r\\|R\\)$" . R-mode)
        ("\\.\\(c\\|h\\|cpp\\)$" . c++-mode)
        ("\\(Makefile\\|makefile\\)$" . makefile-mode)
        ("\\.lua$" . lua-mode)
        ("\\.sh$" . sh-mode)
        ("\\.m$" . octave-mode)
        ("\\.ly$" . LilyPond-mode)
        ("\\.\\(md\\|markdown\\)$" . markdown-mode)
        ("\\.\\(m\\|mod\\)$" . matlab-mode)))

(add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
(add-hook 'LaTeX-mode-hook 'font-lock-mode)
(add-hook 'bibtex-mode-hook 'font-lock-mode)
(add-hook 'R-mode-hook 'font-lock-mode)
(add-hook 'python-mode 'font-lock-mode)

;; disable useless question while quiting emacs
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

;; Stocker les fichiers~ dans un dossier et les supprimer régulièrement
(setq make-backup-files t)
(setq backup-by-copying t)
(setq backup-dir "~/.tmp/emacs/backup/")
(setq auto-save-file-name-transforms `((".*" ,"~/.tmp/emacs/backup/" t)))
(setq tramp-auto-save-directory "~/.tmp/emacs/backup/")
(setq delete-old-versions t)
(add-to-list 'backup-directory-alist `(".*" . ,backup-dir))
(suppression-automatique-demarrage)


(setq abbrev-file-name  "~/.tm/abbrev/abbrev_defs") ; Abbreviations file
(fset 'yes-or-no-p 'y-or-n-p)                       ; simple questions
(ido-mode t)                                        ; Iteractively do things
(prefer-coding-system 'utf-8)                       ; Enable utf-8 by default
(setq ido-file-extensions-order '(".tex" ".el" ".py" ".R"))    ; order files in the mini-buffer

;; ---- Ispell ----

(setq ispell-program-name "aspell")
(setq ispell-dictionary "francais")
(setq ispell-list-command "--list")
(setq flyspell-issue-message-flag nil)






















;; ---- Lua ----

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(setq lua-indent-level 4)

;; ---- Scilab ----

;; (load "scilab-startup")

;; ---- Octave ----

;; (autoload 'octave-mode "octave-mod" nil t)

;; ---- matlab-mode ----

(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")
(when (executable-find "matlab")
  (require 'matlab))

;; ---- END ----

(kill-buffer "*Compile-Log*")
(kill-buffer "*ESS*")
