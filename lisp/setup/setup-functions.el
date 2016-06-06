;; --------------------------------------------------------------------------------
;; Filename : setup-functions.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : setup file for general functions.
;; --------------------------------------------------------------------------------

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

(defun compile-current-window ()
  "Byte-compile le buffer courrant"
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (save-buffer)
    (byte-compile-file buffer-file-name)))

(defun compile-all-files ()
  "Byte-compile le buffer courrant"
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (save-buffer)
    (byte-recompile-directory (expand-file-name "~/.emacs.d/site-lisp") 0)))


(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun cut-region ()
  "Cut the current region"
  (interactive)
  (setq my-text (buffer-substring (region-beginning) (region-end)))
  (delete-region (region-beginning) (region-end)))

(defun cut-line ()
  "Cut the current line"
  (interactive)
  (setq my-text (thing-at-point 'line))
  (delete-current-line))

(defun cut-line-or-region ()
  "Cut the current line or region"
  (interactive)
  (if (use-region-p)
      (cut-region)
    (cut-region)))

(defun copie-ligne-ou-region ()
  "Copie la ligne ou la région actuelle"
  (interactive)
  (if (use-region-p)
      (setq my-text (buffer-substring (region-beginning) (region-end)))
    (setq my-text (thing-at-point 'line))))

(defun my/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;; I took that usefull function from http://lopez-ibanez.eu/dotemacs.html
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun my-comment-dwim ()
  "add a final space to the comment-dwim command."
  (interactive)
  (comment-dwim nil)
  (insert " "))


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

(defun split-auto ()
  "Spliter automatiquement la fenêtre"
  (interactive)
  (if (or (< 160 (window-width))
          (< (* 2 (window-height)) (window-width)))
      (split-droite)
    (split-bas)))

(defun deplacer-fenetre-droite ()
  "Déplacer horizontalement le buffer à droite"
  (interactive)
  (save-selected-window
  (setq my-buffer-origine (current-buffer))
  (windmove-right)
  (setq my-buffer-destination (current-buffer))
  (switch-to-buffer my-buffer-origine))
  (switch-to-buffer my-buffer-destination)
  (windmove-right))

(defun deplacer-fenetre-gauche ()
  "Déplacer horizontalement le buffer à gauche"
  (interactive)
  (save-selected-window
  (setq my-buffer-origine (current-buffer))
  (windmove-left)
  (setq my-buffer-destination (current-buffer))
  (switch-to-buffer my-buffer-origine))
  (switch-to-buffer my-buffer-destination)
  (windmove-left))

(defun deplacer-fenetre-bas ()
  "Déplacer verticalement le buffer vers le bas"
  (interactive)
  (save-selected-window
  (setq my-buffer-origine (current-buffer))
  (windmove-down)
  (setq my-buffer-destination (current-buffer))
  (switch-to-buffer my-buffer-origine))
  (switch-to-buffer my-buffer-destination)
  (windmove-down))

(defun deplacer-fenetre-haut ()
  "Déplacer verticalement le buffer vers le haut"
  (interactive)
  (save-selected-window
  (setq my-buffer-origine (current-buffer))
  (windmove-up)
  (setq my-buffer-destination (current-buffer))
  (switch-to-buffer my-buffer-origine))
  (switch-to-buffer my-buffer-destination)
  (windmove-up))

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

(defun fermer-fenetre ()
  "Fermer la fenêtre courante"
  (interactive)
  (delete-window))

;; Move lines or region
;; This is from http://lopez-ibanez.eu/dotemacs.html

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col))
  (indent-according-to-mode))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current region up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current region down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (start end n)
  (interactive "r\np")
  (if (region-active-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (start end n)
  (interactive "r\np")
  (if (region-active-p) (move-region-down start end n) (move-line-down n)))

;; --- Terminal ----

(defun exist-terminal-p ()
  "Renvoie t si le buffer *terminal* existe et nil sinon"
  (interactive)
  (buffer-live-p (get-buffer "*terminal*")))

(defun ouverture-terminal ()
  "Ouverture efficace d'un terminal"
  (interactive)
  (split-auto)
  (term "bash")
  (term-mode)
  (term-char-mode)
  (global-set-key (kbd "C-c t") 'fermeture-terminal))

(defun aller-terminal ()
  "Aller à la fenêtre de terminal"
  (interactive)
  (setq window (get-buffer-window "*terminal*"))
  (select-window window))

(defun windnew-terminal ()
  "Recherche ou ouvre un terminal"
  (interactive)
  (if (exist-terminal-p)
      (aller-terminal)
    (ouverture-terminal)))

(defun fermeture-terminal ()
  "Fermeture efficace d'un terminal"
  (interactive)
  (term-send-string "*terminal*" "exit")
  (kill-buffer-and-window)
  (global-set-key (kbd "C-c C-t") 'ouverture-terminal))

(defadvice fermeture-terminal (around stfu compile activate)
  "Retirer la question 'un processus existe...' à l'exécution de fermeture-terminal"
  (flet ((yes-or-no-p (&rest args) t)
         (y-or-n-p (&rest args) t))
    ad-do-it))

;; ---- Fill-mode ----

(defun fill-start ()
  "Boucle de démarrage de fill-mode, à ajouter dans les hook"
  (auto-fill-mode 1)
  (setq default-justification 'full))

;; ---- C-mode ----

(defun mon-c-compilation ()
  "Compilation du buffer courrant avec g++"
  (interactive)
  (save-buffer)
  (let ((file (file-name-nondirectory buffer-file-name)))
	    (compile (concat "g++ " file " -o "  (file-name-sans-extension file)))))

;; ---- Lua ----

(defun lua-send-line-or-region ()
  "Interprète une partie de code, que ce soit une ligne ou une région"
  (interactive)
  (if (use-region-p)
      (lua-send-region (region-beginning) (region-end))
    (lua-send-current-line)))


(provide 'setup-functions)
