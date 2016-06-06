;; --------------------------------------------------------------------------------
;; Filename : setup-keys.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : setup file for personnal key bindings.
;; C-f is my universal access to mode's personal funcions
;; --------------------------------------------------------------------------------

(global-set-key (kbd "C-x C-;") 'comment-region)
(global-set-key (kbd "C-x C-:") 'uncomment-region)
(global-set-key (kbd "M-;") 'my-comment-dwim)

(global-set-key (kbd "<C-backspace>") 'undo)
(global-set-key (kbd "<C-M-backspace>") 'redo)

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

(global-set-key (kbd "M-p") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)

(global-set-key (kbd "C-c I") 'load-user-init-file)
(global-set-key (kbd "C-c C-l") 'demarrer-command-log)
(global-set-key (kbd "C-c C-b") 'compile-current-window)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-t") 'windnew-terminal)

(global-set-key (kbd "<C-s-XF86AudioRaiseVolume>") 'flyspell-mode)
(global-set-key (kbd "<C-M-XF86AudioRaiseVolume>") 'flyspell-buffer)


;; --------------------------------------------------------------------------------
;;     Lua-mode
;; --------------------------------------------------------------------------------

(defun mon-lua-mode-map ()
  "Définition des raccourcis clavier du lua-mode"
  (local-set-key (kbd "<C-return>") 'lua-send-line-or-region)
  (local-set-key (kbd "<C-M-return>") 'lua-send-buffer))

(add-hook 'lua-mode-hook 'mon-lua-mode-map)

;; --------------------------------------------------------------------------------
;;     Terminal
;; --------------------------------------------------------------------------------

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))


(provide 'setup-keys)
