;; --------------------------------------------------------------------------------
;; Filename : setup-packages.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : setup file for packages installation.
;; --------------------------------------------------------------------------------

(require 'package)

;; Packages
(eval-after-load "package"
  '(progn
     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
     (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
     (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)     
     (package-initialize)))
(setq package-user-dir "~/.emacs.d/site-lisp")

; list the packages you want
;(setq package-list '(auctex ac-math async auto-complete auto-complete-auctex
;                            autopair color-theme company company-auctex
;                            company-c-headers company-jedi company-math
;                            company-quickhelp company-statistics concurrent ctable
;                            dash deferred epc ess f fill-column-indicator git-commit
;                            jedi jedi-core julia-mode lua-mode magit magit-popup
;                            markdown-mode math-symbol-lists mwe-log-commands popup
;                            pos-tip python-environment pythonic python-mode s
;                            savekill smartparens with-editor yasnippet))

; fetch the list of packages available 
;(unless package-archive-contents
;  (package-refresh-contents))

; install the missing packages
;(dolist (package package-list)
;  (unless (package-installed-p package)
;    (package-install package)))

(provide 'setup-packages)
