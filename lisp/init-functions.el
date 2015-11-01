;; Pierre-Antoine ROBERT <pierreantoine dot robert at gmail dot com>

;; ---- Généraux

(defun load-user-init-file ()
  "Éditer rapidement le fichier init."
  (interactive)
  (find-file-other-window user-init-file))

(defun suppression-automatique-demarrage ()
  "Suppression automatique des vielles sauvegardes de fichiers édités sur 
emacs."
  (message "Suppression des vielles sauvegardes...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (fifth (file-attributes file))))
                    week))
        (delete-file file)))))

(defun my-fill-nobreak-predicate ()
  "Ne pas revenir à la ligne n'importe où"
  (save-match-data
    (or (looking-at "[ \t]*[])}»!?;:]")
        (looking-at "[ \t]*\\.\\.\\.")
        (save-excursion
          (skip-chars-backward " \t")
          (backward-char 1)
          (looking-at "[([{«]")))))

(defun demarrer-command-log ()
  "Demarrer la command-log"
  (interactive)
  (mwe:log-keyboard-commands)
  (mwe:open-command-log-buffer)
  (other-window -1))

(defun compile-fenetre-actuelle ()
  "Byte-compile le buffer courrant"
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (save-buffer)
    (byte-compile-file buffer-file-name)))

;; ---- spliter la fenêtre ----

(defun split-droite ()
  "Split horizontalement la fenêtre vers la droite"
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "*scratch*"))

(defun split-gauche ()
  "Split horizontalement la fenêtre vers la gauche"
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer "*scratch*"))

(defun split-bas ()
  "Split verticalement la fenêtre vers le bas"
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*scratch*"))

(defun split-haut ()
  "Split verticalement la fenêtre vers le haut"
  (interactive)
  (split-window-vertically)
  (switch-to-buffer "*scratch*"))

(defun fermer-fenetre ()
  "Fermer la fenêtre courante"
  (interactive)
  (delete-window))

(defun split-auto ()
  "Spliter automatiquement la fenêtre"
  (interactive)
  (if (or (< 160 (window-width))
          (< (* 2 (window-height)) (window-width)))
      (split-droite)
    (split-bas)))

;; Déplacer une fenêtre
;; (defun deplace-droite ()
;;   "déplacer horizontalement la fenêtre à droite"
;;   (interactive)

(defun elargir-fenetre-horizontalement ()
  "Elargir horizontalement d'une colonne la fenêtre"
  (interactive)
  (enlarge-window-horizontally 1))

(defun retrecir-fenetre-horizontalement ()
  "Rétrécir horizontalement d'une colone la fenêtre"
  (interactive)
  (shrink-window-horizontally 1))

(defun elargir-fenetre-verticalement ()
  "Elargir verticalement d'une colonne la fenêtre"
  (interactive)
  (enlarge-window 1))

(defun retrecir-fenetre-verticalement ()
  "Rétrécir verticallement d'une colone la fenêtre"
  (interactive)
  (shrink-window 1))

;; ---- Python ----

(defun exist-ipython-p ()
  "Renvoie t si le buffer *ipython* existe et nil sinon"
  (interactive)
  (buffer-live-p (get-buffer "*ipython*")))
  
(defun load-ipython (&optional filename)
  "Create a new Ipython buffer, optionnaly running a given file"
  (interactive)
  (set-buffer
   (make-term "ipython" "/usr/bin/env" nil "ipython" (or filename "")
              "--TerminalIPythonApp.force_interact=True"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*ipython*")
  (setq-local origin filename))

(defun reload-ipython (&optional filename)
  (interactive)
  (setq window (get-buffer-window "*ipython*"))
  (if window
      (select-window window)
    (split-auto)
    (set-buffer "*ipython*"))
  (if (not filename)
      (setq filename origin))
  (kill-buffer)
  (load-ipython filename))

(defun windnew-ipython ()
  "Exécute le buffer courrant dans un terminal ipython"
  (interactive)
  (save-buffer)
  (if (exist-ipython-p)
      (reload-ipython (buffer-file-name))
    (split-auto)
    (load-ipython (buffer-file-name))))

(defun copie-ligne-ou-region ()
  "Copie la ligne ou la région actuelle"
  (interactive)
  (if (use-region-p)
      (setq my-text (buffer-substring (region-beginning) (region-end)))
    (setq my-text (thing-at-point 'line))))

(defun execute-ipython (code)
  "Exécute du code copié dans un terminal ipython"
  (term-mode)
  (term-line-mode)
  (insert code)
  (term-char-mode))

(defun interprete-ipython ()
  "Interprète une partie de code"
  (interactive)
  (save-excursion
  (copie-ligne-ou-region)
  (if (not 'exist-ipython-p)
      (load-ipython "")
    (setq window (get-buffer-window "*ipython*"))
    (select-window window))
  (execute-ipython my-text)))


;; ---- Company-mode ----

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

;; ---- Fill-mode ----

(defun fill-start ()
  "Boucle de démarrage de fill-mode, à ajouter dans les hook"
  (auto-fill-mode 1)
  (setq default-justification 'full))

;; ---- LaTeX-mode ----

(defun my-latex-mode-setup ()
  "Boucle de démarrage du mode LaTeX"
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))

;; ---- C-mode ----

(defun mon-c-compilation ()
  "Compilation du buffer courrant avec g++"
  (interactive)
  (save-buffer)
  (let ((file (file-name-nondirectory buffer-file-name)))
	    (compile (concat "g++ " file " -o "  (file-name-sans-extension file)))))
