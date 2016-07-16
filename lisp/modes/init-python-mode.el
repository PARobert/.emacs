;; -----------------------------------------------------------------------------
;; Filename : init-python-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for python-mode.
;; -----------------------------------------------------------------------------

(require 'python-mode)

;; -----------------------------------------------------------------------------
;;     Functions
;; -----------------------------------------------------------------------------

(defun exist-ipython-p ()
  "Renvoie t si le buffer *ipython* existe et nil sinon"
  (interactive)
  (buffer-live-p (get-buffer "*ipython*")))
  
(defun load-ipython (version &optional filename)
  "Create a new Ipython buffer, optionnaly running a given file."
  (interactive)
  (set-buffer
   (pcase version
     ("2" (make-term "ipython" "/usr/bin/env" nil "ipython" (or filename "")
                    "--no-banner" "--no-autoindent" "--TerminalIPythonApp.force_interact=True"))
     ("3" (make-term "ipython" "/usr/bin/env" nil "ipython3" (or filename "")
                    "--no-banner" "--no-autoindent" "--TerminalIPythonApp.force_interact=True"))))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*ipython*"))

(defun reload-ipython (version &optional filename)
  "Relancer le terminal ipython"
  (interactive)
  (setq window (get-buffer-window "*ipython*"))
  (if window
      (select-window window)
    (split-auto)
    (set-buffer "*ipython*"))
  (kill-buffer)
  (load-ipython version filename))

(defadvice reload-ipython (around stfu compile activate)
  "Retirer la question 'un processus existe...' à l'exécution de reload-ipython"
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(defun windnew-ipython (&optional version)
  "Exécute le buffer courrant dans un terminal ipython"
  (interactive)
  (save-buffer)
  (if (exist-ipython-p)
      (reload-ipython python-version (buffer-file-name))
    (setq origine-python (get-buffer-window (current-buffer)))
    (split-auto)
    (load-ipython version (buffer-file-name))
    (setq python-version version))
  (select-window origine-python))

(defun windnew-ipython-2 ()
  "Exécute le buffer courrant dans un terminal ipython 2"
  (interactive)
  (windnew-ipython "2"))
  
(defun windnew-ipython-3 ()
  "Exécute le buffer courrant dans un terminal ipython 3"
  (interactive)
  (windnew-ipython "3"))

(defun execute-ipython (proc code)
  "Exécute du code copié dans un terminal ipython"
  (term-send-string proc code))

(defun interprete-ipython ()
  "Interprète une partie de code"
  (interactive)
  (save-excursion
  (copie-ligne-ou-region)
  (if (not 'exist-ipython-p)
      (load-ipython "")
    (setq window (get-buffer-window "*ipython*")))
  (execute-ipython "*ipython*" my-text))
  (forward-line))


;; -----------------------------------------------------------------------------
;;     Skeleton-abbrev
;; -----------------------------------------------------------------------------

;; (define-skeleton python-equal
;;   "insert spaces befor and after \'=\'"
;;   nil
;;   (if (not (looking-at "[:space:]"))
;;       "= " -
;;       -))

;; (define-skeleton python-equal
;;   "insert spaces befor and after \'=\'"
;;   nil " = " -)

;; (define-abbrev python-mode-abbrev-table "=" "" 'python-equal)


;; -----------------------------------------------------------------------------
;;     Mode-map
;; -----------------------------------------------------------------------------

(defun mon-python-mode-map ()
  "Key bindings for python-mode."
  (local-unset-key (kbd "C-c C-\""))
  (local-set-key (kbd "C-c |") 'nil)
  (local-set-key (kbd "\C-m") 'newline-and-indent)
  (local-set-key (kbd "<C-tab>") 'py-indent-line)  
  (local-set-key (kbd "<C-return>") 'interprete-ipython)
  (local-set-key (kbd "C-c C-é") 'windnew-ipython-2)
  (local-set-key (kbd "C-c C-\"") 'windnew-ipython-3)
  (local-set-key (kbd "<C-M-return>") 'windnew-ipython))

(add-hook 'python-mode-hook 'mon-python-mode-map)


;; -----------------------------------------------------------------------------

(provide 'init-python-mode)
