;; --------------------------------------------------------------------------------
;; Filename : init-tmp-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for ess-mode.
;; --------------------------------------------------------------------------------

(require 'ess)

(autoload 'R-mode "ess-site.el" "ESS" t)
(add-hook 'ess-mode-hook '(company-statistics-mode t))

(setq ess-ask-for-ess-directory nil)
(setq comint-input-ring-size 1000)
(setq ess-indent-level 4)
(setq ess-arg-function-offset 4)
(setq ess-else-offset 4)

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun mon-R-eval ()
  "[old] Pass code to an interpreter from a line or a region"
  (interactive)
  (if (use-region-p)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

;; (defun mon-R-eval ()
;;   "[new] Pass code to an interpreter from a line or a region"
;;   (interactive)
;;   (if (use-region-p)
;;       (let ((my-begin (region-beginning))
;;             (my-end (region-end)))
;;         (goto-char (my-begin))
;;         (acti)
;;         (while (< (point) (region-end))
;;           (ess-eval-region-and-go (region-beginning) (region-end))
;;           (call-interactively 'ess-eval-line-and-step)))))
;;     ;; (call-interactively 'ess-eval-line-and-step))))


;; --------------------------------------------------------------------------------
;;      Key-bindings
;; --------------------------------------------------------------------------------

(defun mon-R-mode-map ()
  "Définition des raccourcis clavier du R-mode"
  (local-set-key (kbd "<C-return>") 'mon-R-eval)
  (local-unset-key (kbd "<->")))

(add-hook 'R-mode-hook 'mon-R-mode-map)
