;; --------------------------------------------------------------------------------
;; Filename : setup-tmp.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : Setup for my personnal headers.
;; This come from http://emacs.stackexchange.com/a/11010/10056
;; --------------------------------------------------------------------------------

(require 'header2)

(defvar current-date-time-format "%a %d %b %Y %H:%M:%S")
(defvar header-name "Pierre-Antoine ROBERT")
(defvar header-email-adress "<pierre.antoine.ROBERT@ensae-paristech.fr>")

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

(defsubst my/header-dash-line ()
  "Insert dashed line."
  (insert header-prefix-string)
  (insert-char ?- fill-column)
  (insert "\n"))

(defsubst my/header-blank ()
  "Insert a blank line without prefix."
  (insert "\n"))

(setq make-header-hook '(my/header-dash-line
                         my/header-file-name
                         my/header-author
                         header-blank
                         my/header-description
                         my/header-dash-line
                         my/header-blank))

(add-hook 'emacs-lisp-mode-hook #'auto-make-header)


(provide 'setup-header)
