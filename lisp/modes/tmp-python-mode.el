;; ---- Python ----

(defun exist-ipython-p ()
  "Renvoie t si le buffer *ipython* existe et nil sinon"
  (interactive)
  (buffer-live-p (get-buffer "*ipython*")))
  
(defun load-ipython (&optional filename)
  "Create a new Ipython buffer, optionnaly running a given file"
  (interactive)
  (set-buffer
   ;; (make-term "ipython" "/usr/bin/env" nil "ipython" (or filename "")
   ;;            "--TerminalIPythonApp.force_interact=True"))
   ;; ou :
   ;;
   ;; (make-term "ipython" "/usr/bin/python3" nil))
   (make-term "ipython" "/usr/bin/env" nil "ipython3" (or filename "")
              "--TerminalIPythonApp.force_interact=True"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*ipython*")
  (setq-local origin-python filename))

(defun reload-ipython (&optional filename)
  "Relancer le terminal ipython"
  (interactive)
  (setq window (get-buffer-window "*ipython*"))
  (if window
      (select-window window)
    (split-auto)
    (set-buffer "*ipython*"))
  (if (not filename)
      (setq filename origin-python))
  (kill-buffer)
  (load-ipython filename))

(defadvice reload-ipython (around stfu compile activate)
  "Retirer la question 'un processus existe...' à l'exécution de reload-ipython"
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

(defun windnew-ipython ()
  "Exécute le buffer courrant dans un terminal ipython"
  (interactive)
  (save-buffer)
  (save-window-excursion
  (if (exist-ipython-p)
      (reload-ipython (buffer-file-name))
    (split-auto)
    (load-ipython (buffer-file-name)))))

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
  (next-line))
