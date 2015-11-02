;; Pierre-Antoine ROBERT <pierreantoine dot robert at gmail dot com>

;; Généraux

(global-set-key (kbd "C-x C-;") 'comment-region)
(global-set-key (kbd "C-x C-:") 'uncomment-region)

(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "<s-backspace>") 'fermer-fenetre)

(global-set-key (kbd "<M-right>") 'split-droite)
(global-set-key (kbd "<M-left>") 'split-gauche)
(global-set-key (kbd "<M-down>") 'split-bas)
(global-set-key (kbd "<M-up>") 'split-haut)

(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-up>") 'windmove-up)

(global-set-key (kbd "<C-s-right>") 'retrecir-fenetre-horizontalement)
(global-set-key (kbd "<C-s-left>") 'elargir-fenetre-horizontalement)
(global-set-key (kbd "<C-s-down>") 'retrecir-fenetre-verticalement)
(global-set-key (kbd "<C-s-up>") 'elargir-fenetre-verticalement)

(global-set-key (kbd "<M-s-right>") 'deplacer-fenetre-droite)
(global-set-key (kbd "<M-s-left>") 'deplacer-fenetre-gauche)
(global-set-key (kbd "<M-s-down>") 'deplacer-fenetre-bas)
(global-set-key (kbd "<M-s-up>") 'deplacer-fenetre-haut)

(global-set-key (kbd "C-c I") 'load-user-init-file)
(global-set-key (kbd "C-c C-l") 'demarrer-command-log)
(global-set-key (kbd "C-c C-b") 'compile-fenetre-actuelle)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-t") 'windnew-terminal)


;; ---- Python-mode ----

(defun mon-python-mode-map ()
  "Définition des raccourcis clavier du python-mode"
  (local-set-key (kbd "C-c |") 'nil)
  (local-set-key (kbd "<C-return>") 'interprete-ipython)
  (local-set-key (kbd "<C-M-return>") 'windnew-ipython)
  (local-set-key (kbd "\C-m") 'newline-and-indent)
  (local-set-key (kbd "<C-tab>") 'nil))
(add-hook 'python-mode-hook 'mon-python-mode-map)

;; ---- Company ----

(global-set-key (kbd "\t") 'company-complete-common)

;; ---- C-mode ----

(defun mon-c++-mode-map ()
  "Définition des raccourcis clavier du C++-mode"
  (local-set-key (kbd "C-c C-c") 'mon-c-compilation)
  (local-set-key (kbd "<return>") 'newline-and-indent))
(add-hook 'c++-mode-hook 'mon-c++-mode-map)
