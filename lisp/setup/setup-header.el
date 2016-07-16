;; --------------------------------------------------------------------------------
;; Filename : setup-tmp.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : Setup for my personnal headers.
;; This come from http://emacs.stackexchange.com/a/11010/10056
;; --------------------------------------------------------------------------------

(require 'header2)


;; --------------------------------------------------------------------------------
;;     Variables
;; --------------------------------------------------------------------------------

(defvar current-date-time-format "%a %d %b %Y %H:%M:%S")
(defvar header-name "Pierre-Antoine ROBERT")
(defvar header-email-adress "<pierre.antoine.ROBERT@ensae-paristech.fr>")

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defsubst my/header-timestamp ()
  "Insert field for timestamp."
  (insert header-prefix-string  "Date : ")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))

(defsubst my/header-file-name ()
  "Insert \"Filename : \" line, using buffer's file name."
  (insert header-prefix-string "Filename : "
          (if (buffer-file-name)
              (file-name-nondirectory (buffer-file-name))
            (buffer-name))
          "\n"))

(defsubst my/header-author ()
  "Insert \"Author : \" line."
  (insert header-prefix-string)
  (insert "Author : ")
  (insert header-name " ")
  (insert header-email-adress "\n"))

(defsubst my/header-description ()
  "Insert \"Description : \" line."
  (insert header-prefix-string "Description : \n"))

(defsubst my/header-location ()
  "Insert \"Location : \" line."
  (insert header-prefix-string "Location : \n"))

(defsubst my/header-dash-line ()
  "Insert dashed line."
  (insert (header-prefix-string))
  (insert-char ?- (- fill-column (length (header-prefix-string))))
  (insert "\n"))

(defsubst my/header-blank ()
  "Insert a blank line without prefix."
  (insert "\n"))

(defsubst my/header-blank-line ()
  "Insert an empty comment to file header (after `header-prefix-string')."
  (insert header-prefix-string  "\n"))

(defun my/default-header ()
  "Default header for all modes."
  (setq-local make-header-hook '(my/header-dash-line
                                 my/header-file-name
                                 my/header-author
                                 my/header-blank-line
                                 my/header-description
                                 my/header-dash-line
                                 my/header-blank)))

(defun my/eviews-mode-header ()
  "Specific header for eviews-mode."
  (setq-local make-header-hook '(my/header-dash-line
                                 my/header-file-name
                                 my/header-author
                                 my/header-blank-line
                                 my/header-description
                                 my/header-blank-line
                                 my/header-location
                                 my/header-dash-line
                                 my/header-blank)))

;; --------------------------------------------------------------------------------
;;     Hooks
;; --------------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook #'auto-make-header)
(add-hook 'LaTeX-mode-hook #'auto-make-header)
(add-hook 'R-mode-hook #'auto-make-header)
(add-hook 'python-mode-hook #'auto-make-header)
(add-hook 'eviews-mode-hook #'auto-make-header)

(add-hook 'emacs-lisp-mode-hook 'my/default-header)
(add-hook 'LaTeX-mode-hook 'my/default-header)
(add-hook 'R-mode-hook 'my/default-header)
(add-hook 'python-mode-hook 'my/default-header)
(add-hook 'eviews-mode-hook 'my/eviews-mode-header)


;; --------------------------------------------------------------------------------

(provide 'setup-header)
