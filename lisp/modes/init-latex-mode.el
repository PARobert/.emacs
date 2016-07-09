;; --------------------------------------------------------------------------------
;; Filename : init-tmp-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for latex/auctex mode.
;; --------------------------------------------------------------------------------

(require 'latex)
(require 'font-latex)

(load "auctex.el" nil t t)
(autoload 'flyspell-babel-setup "flyspell-babel")
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)

(add-hook 'latex-mode-hook 'flyspell-babel-setup)
(add-hook 'LaTeX-mode-hook 'flyspell-prog-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'fill-start)
(add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)

(company-auctex-init)

(setq-local company-backends (append '(company-math-symbols-latex)))
(setq-local company-backends (append '(company-latex-commands)))
(setq-local company-backends (append '(company-backends)))
(setq TeX-show-compilation nil)
(setq TeX-electric-sub-and-superscript t) 

(defvar my-text)

;; --------------------------------------------------------------------------------
;;     Local theme
;; --------------------------------------------------------------------------------

(set-face-attribute 'font-latex-sectioning-0-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)
(set-face-attribute 'font-latex-sectioning-1-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)
(set-face-attribute 'font-latex-sectioning-2-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)
(set-face-attribute 'font-latex-sectioning-3-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)
(set-face-attribute 'font-latex-sectioning-4-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)
(set-face-attribute 'font-latex-sectioning-5-face nil
                    :foreground my-lf :height 98 :bold t :inherit nil)

(set-face-attribute 'font-latex-bold-face nil
                    :foreground my-lp :bold t :italic nil :inherit nil)
(set-face-attribute 'font-latex-warning-face nil
                    :foreground my-lp :bold t :italic nil :inherit nil)
(set-face-attribute 'font-latex-italic-face nil
                    :foreground my-lp :bold t :italic nil :inherit nil)
(set-face-attribute 'font-latex-string-face nil
                    :foreground my-lw :bold nil :italic nil :inherit nil)
(set-face-attribute 'font-latex-verbatim-face nil
                    :foreground my-lw :bold nil :italic nil :inherit nil)
(set-face-attribute 'font-latex-doctex-preprocessor-face nil
                    :foreground my-lw :bold nil :italic nil :inherit nil)
(set-face-attribute 'font-latex-doctex-documentation-face nil
                    :foreground my-lw :bold nil :italic nil :inherit nil)
(set-face-attribute 'font-latex-subscript-face nil
                    :foreground my-lw :bold nil :italic nil :height 98 :inherit nil)
(set-face-attribute 'font-latex-superscript-face nil
                    :foreground my-lw :bold nil :italic nil :height 98 :inherit nil)

(setq font-latex-script-display (quote ((raise 0) raise 0)))

;; (eval-after-load "font-latex"
;;   '(font-latex-add-keywords '(("newenvironment" "*{[[")
;;                   ("renewenvironment" "*{[[")
;;                   ("newcommand" "*|{\\[[")
;;                   ("renewcommand" "*|{\\[[")
;;                   ("providecommand" "*|{\\[[")
;;                   ("fbox" "")
;;                   ("mbox" "")
;;                   ("sbox" ""))
;;                             'function))
;;   "Add KEYWORDS to CLASS.
;; KEYWORDS is a list of keywords or keywords with syntax specs.
;; CLASS corresponds to a keyword class and can be one of the
;; symbols 'warning, 'variable, 'reference, 'biblatex, 'function,
;; 'sectioning-0, 'sectioning-1, 'sectioning-2, 'sectioning-3,
;; 'sectioning-4, 'sectioning-5, 'slide-title, 'textual,
;; 'bold-command, 'italic-command, 'math-command, 'type-command,
;; 'bold-declaration, 'italic-declaration or 'type-declaration.

;; (eval-after-load 'LaTeX-mode
;;   '(font-latex-add-keywords
;;     '(("citeauthor" "")
;;       ("citeA" ""))
;;     'function))

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun latex-insert-eq ()
  "Insert an aqnarray environment"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\begin{eqnarray}\n" my-text "\n\\end{eqnarray}"))
    (insert "\\begin{eqnarray}\n" "\n\\end{eqnarray}"))
  (forward-line -1))

(defun latex-insert-it ()
  "Insert \textit"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\textit{" my-text "}"))
    (progn
      (insert "\\textit{}")
      (backward-char))))

(defun latex-insert-bf ()
  "Insert \textbf"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\textbf{" my-text "}"))
    (progn
      (insert "\\textbf{}")
      (backward-char))))

(defun latex-insert-center ()
  "Insert a center environment"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\begin{center}\n" my-text "\n\\end{center}"))
    (insert "\\begin{center}\n" "\n\\end{center}"))
  (forward-line -1))

(defun latex-insert-flushright ()
  "Insert a flushright environment"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\begin{flushright}\n" my-text "\n\\end{flushright}"))
    (insert "\\begin{flushright}\n" "\n\\end{flushright}"))
  (forward-line -1))

(defun latex-insert-flushleft ()
  "Insert a flushleft environment"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\begin{flushleft}\n" my-text "\n\\end{flushleft}"))
    (insert "\\begin{flushleft}\n" "\n\\end{flushleft}"))
  (forward-line -1))

(defun latex-insert-figure ()
  "Insert a figure environment"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "\\begin{figure}[h!]\n\\centering\n\\includegraphics{" my-text
                "}\n\\label{}\n\\caption{}\n\\end{figure}"))
    (insert "\\begin{figure}[h!]\n\\centering\n\\includegraphics{}"
            "\n\\label{}\n\\caption{}\n\\end{figure}"))
  (forward-line -3)
  (end-of-line)
  (backward-char))

(defun latex-insert-table ()
  "Insert a table environment"
  (interactive)
  (insert "\\begin{table}[h!]\n\\centering\n\\begin{tabularx}{\\textwidth}{}\n\n"
            "\\end{tabularx}\n\\label{}\n\\caption{}\n\\end{table}")
  (forward-line -5)
  (end-of-line)
  (backward-char))

(defun latex-insert-new ()
  "Insert a new document"
  (interactive)
  (insert "\\documentclass[12pt,a4paper,article]{memoir}\n"
          "\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n"
          "\\usepackage{lmodern,textcomp}\n\\usepackage[french]{babel}\n"
          "\\usepackage{apacite}\n\\usepackage{geometry}\n\n"
          "%-- Mise en page\n\\geometry{hmargin=3.5cm, vmargin=2.5cm}"
          "  % Réglage des marges\n\\setlength{\\parskip}{0.4cm}"
          "  % Interligne entre paragraphes\n\\renewcommand{\\baselinestretch}{1}"
          " % Interligne\n\\renewcommand{\\thesection}{\\arabic{section}}"
          " % Modification du titre de section\n\n\\begin{document}"
          "\n\n\\end{document}")
  (forward-line -1))

(defun latex-insert-right-par ()
  "Insert \right)"
  (interactive)
  (insert "\\right)"))

(defun latex-insert-left-par ()
  "Insert \left("
  (interactive)
  (insert "\\left("))

(defun latex-insert-right-cro ()
  "Insert \right]"
  (interactive)
  (insert "\\right]"))

(defun latex-insert-left-cro ()
  "Insert \left["
  (interactive)
  (insert "\\left["))

(defun latex-insert-expo ()
  "Insert ^{}"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "^{" my-text "}"))
    (progn
      (insert "^{}")
      (backward-char))))

(defun latex-insert-ind ()
  "Insert _{}"
  (interactive)
  (if (use-region-p)
      (progn
        (cut-region)
        (insert "_{" my-text "}"))
    (progn
      (insert "_{}")
      (backward-char))))

(defun run-latexmk ()
  "Compiles the current document"
  (interactive)
  (let ((TeX-save-query nil)
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
         (TeX-command-expand "latexmk -pdf %t" 'TeX-master-file)
         master-file)
    (minibuffer-message "latexmk done")))

;; --------------------------------------------------------------------------------
;;     Mode-map
;; --------------------------------------------------------------------------------

(defun my-latex-mode-map ()
  "Short-cuts for LaTeX-mode : to be finished"
  (local-unset-key (kbd "C-s-l"))
  (local-set-key (kbd "C-s-i") 'latex-insert-it)
  (local-set-key (kbd "C-s-b") 'latex-insert-bf)
  (local-set-key (kbd "C-s-(") 'latex-insert-left-par)
  (local-set-key (kbd "C-s-)") 'latex-insert-right-par)
  (local-set-key (kbd "C-s-[") 'latex-insert-left-cro)
  (local-set-key (kbd "C-s-]") 'latex-insert-right-cro)
  (local-set-key (kbd "C-s-_") 'latex-insert-ind)
  (local-set-key (kbd "<C-s-dead-circumflex>") 'latex-insert-expo)
  (local-set-key (kbd "C-s-e C-s-c") 'latex-insert-center)
  (local-set-key (kbd "C-s-e C-s-r") 'latex-insert-flushright)
  (local-set-key (kbd "C-s-e C-s-l") 'latex-insert-flushleft)
  (local-set-key (kbd "C-s-e C-s-e") 'latex-insert-eq)
  (local-set-key (kbd "C-s-e C-s-f") 'latex-insert-figure)
  (local-set-key (kbd "C-s-e C-s-t") 'latex-insert-table)
  (local-set-key (kbd "C-s-e C-s-n") 'latex-insert-new)
  (local-set-key (kbd "C-c C-c") #'run-latexmk))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-map)


(provide 'init-latex-mode)
