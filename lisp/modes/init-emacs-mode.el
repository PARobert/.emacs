;; --------------------------------------------------------------------------------
;; Filename : init-emacs-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for the emacs-lisp-mode.
;; --------------------------------------------------------------------------------

(defvar title-prefix-string ";; ")

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun my-title-dash-line ()
  "Insert dashed line"
  (insert title-prefix-string)
  (insert-char ?- fill-column)
  (insert "\n"))

(defun my-title (title)
  (interactive "sEnter the title : ")
  (my-title-dash-line)
  (insert title-prefix-string)
  (insert "    " title "\n")
  (my-title-dash-line))

;; --------------------------------------------------------------------------------
;;     Mode-map
;; --------------------------------------------------------------------------------

(defun my-emacs-mode-map ()
  "Short-cuts for emacs-mode"
  (local-set-key (kbd "C-s-t") 'my-title))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-mode-map)


