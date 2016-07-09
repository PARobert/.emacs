;; --------------------------------------------------------------------------------
;; Filename : setup-title.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : setuo file for title functionality. It inserts a title in the code
;; to makes it easier to read.
;; --------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------
;;     Variables
;; --------------------------------------------------------------------------------

(defvar title-prefix-string " " "Prefix used as comment symbol for title.")

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(make-variable-buffer-local 'title-prefix-string)

(defun add-dash-line (&optional after)
  "Insert dashed line."
  (or after (setq after " "))
  (insert title-prefix-string)
  (insert-char ?- fill-column)
  (insert after))

(defun add-title (title)
  "Insert a title to structure the code."
  (interactive "sEnter the title : ")
  (if (> (string-width title) (- fill-column 10))
      (progn
        (unwind-protect
            (message "Title is too long")
          (sleep-for 1.0)
          (call-interactively 'add-title)))
    (add-dash-line "\n")
    (insert title-prefix-string "    " title "\n")
    (add-dash-line "\n\n")))

(defun add-dash-line-interactivly ()
  "Insert dashed line."
  (interactive)
  (add-dash-line "\n\n"))

(defun title-map (string)
  "Initialize the title functionality."
  (setq title-prefix-string string)
  (local-set-key (kbd "C-s-t") 'add-title)
  (local-set-key (kbd "C-s-l") 'add-dash-line-interactivly))


;; --------------------------------------------------------------------------------
;;     Mode specific specifications
;; --------------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook (lambda () (title-map ";; ")))
(add-hook 'ess-mode-hook (lambda () (title-map "# ")))
(add-hook 'eviews-mode-hook (lambda () (title-map "' ")))
(add-hook 'LaTeX-mode-hook (lambda () (title-map "% ")))
(add-hook 'latex-mode-hook (lambda () (title-map "% ")))
(add-hook 'python-mode-hook (lambda () (title-map "# ")))
(add-hook 'c++-mode-hook (lambda () (title-map "// ")))

;; --------------------------------------------------------------------------------

(provide 'setup-title)
