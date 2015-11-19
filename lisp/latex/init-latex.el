(load "auctex.el" nil t t)
(load "font-latex")
(tex-font-setup)

;; (custom-set-faces
;;  '(font-latex-warning-face ((t (:inherit bold :foreground "orange red")))))

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             'flyspell-mode
;;             'turn-on-auto-fill
;;             'fill-start))


;; (setq font-latex-built-in-keyword-classes
;;       '(("warning"
;;          ("nopagebreak" "pagebreak" "newpage" "clearpage" "cleardoublepage"
;;           "enlargethispage" "nolinebreak" "linebreak" "newline" "-" "\\" "\\*"
;;           "appendix" "displaybreak" "allowdisplaybreaks" "include")
;;          'font-lock-variable-name-face 1 noarg)
;;         ("variable"
;;          (("setlength" "|{\\{") ("settowidth" "|{\\{") ("settoheight" "{{")
;;           ("settodepth" "{{") ("setcounter" "{|{\\")
;;           ("addtolength" "|{\\{") ("addtocounter" "{|{\\")
;;           ("stepcounter" "{") ("refstepcounter" "{")
;;           ("arabic" "{") ("roman" "{") ("Roman" "{") ("alph" "{") ("Alph" "{")
;;           ("fnsymbol" "{"))
;;          'font-lock-variable-name-face 2 command)
;;         ("biblatexnoarg"
;;          ("newrefsegment" "mancite" "pno" "ppno" "nopp" "psq" "psqq")
;;          'font-lock-variable-name-face 2 noarg)
;;         ("biblatex"
;;          (("newrefsection" "[") ("ExecuteBibliographyOptions" "[{")
;;           ("printbibliography" "[") ("printshorthands" "[") ("printbibheading" "[")
;;           ("addbibresource" "[{") ("addglobalbib" "[{") ("addsectionbib" "[{")
;;           ("bibbysection" "[") ("bibbysegment" "[") ("bibbycategory" "[")
;;           ("DeclareBibliographyCategory" "{") ("addtocategory" "{{") ("defbibenvironment" "{{{{")
;;           ("defbibheading" "{[{") ("defbibnote" "{{") ("defbibfilter" "{{") ("defbibcheck" "{{")
;;           ("defbibentryset" "{{") ("Cite" "[[{") ("parencite" "*[[{") ("Parencite" "[[{")
;;           ("footcite" "[[{") ("footcitetext" "[[{") ("textcite" "[[{") ("Textcite" "[[{")
;;           ("smartcite" "[[{") ("Smartcite" "[[{") ("supercite" "{") ("autocite" "*[[{")
;;           ("Autocite" "*[[{") ("citeauthor" "[[{") ("Citeauthor" "[[{") ("citetitle" "*[[{")
;;           ("citeyear" "*[[{") ("citedate" "*[[{") ("citeurl" "[[{") ("parentext" "{")
;;           ("brackettext" "{") ("fullcite" "[[{") ("fullfootcite" "[[{") ("volcite" "[{[[")
;;           ("Volcite" "[{[[") ("pvolcite" "[{[[") ("Pvolcite" "[{[[") ("fvolcite" "[{[[")
;;           ("ftvolcite" "[{[[") ("svolcite" "[{[[") ("Svolcite" "[{[[") ("tvolcite" "[{[[")
;;           ("Tvolcite" "[{[[") ("avolcite" "[{[[") ("Avolcite" "[{[[") ("notecite" "[[{")
;;           ("Notecite" "[[{") ("pnotecite" "[[{") ("Pnotecite" "[[{") ("fnotecite" "[[{")
;;           ("citename" "[[{[{") ("citelist" "[[{[{") ("citefield" "[[{[{") ("citereset" "*")
;;           ("RN" "{") ("Rn" "{") ("DefineBibliographyStrings" "{{") ("DefineBibliographyExtras" "{{")
;;           ("UndefineBibliographyExtras" "{{") ("DefineHyphenationExceptions" "{{")
;;           ("NewBibliographyString" "{") ("autocites" "(([[{") ("Autocites" "(([[{")
;;           ("cites" "(([[{") ("Cites" "(([[{") ("parencites" "(([[{") ("Parencites" "(([[{")
;;           ("footcites" "(([[{") ("footcitetexts" "(([[{") ("smartcites" "(([[{")
;;           ("Smartcites" "(([[{") ("textcites" "(([[{") ("Textcites" "(([[{") ("supercites" "(([[{"))
;;          'font-lock-constant-face 2 command)
;;         ("reference"
;;          (("nocite" "*{") ("cite" "*[[{") ("label" "{") ("pageref" "{")
;;           ("vref" "{") ("eqref" "{") ("ref" "{") ("include" "{")
;;           ("input" "{") ("bibliography" "{") ("index" "{") ("glossary" "{")
;;           ("footnote" "[{") ("footnotemark" "[") ("footnotetext" "[{"))
;;          'font-lock-constant-face 2 command)
;;         ("function"
;;          (("begin" "{") ("end" "{") ("pagenumbering" "{")
;;           ("thispagestyle" "{") ("pagestyle" "{") ("nofiles" "")
;;           ("includeonly" "{") ("bibliographystyle" "{") ("documentstyle" "[{")
;;           ("documentclass" "[{[") ("newenvironment" "*{[[{{")
;;           ("newcommand" "*|{\\[[{") ("newlength" "|{\\")
;;           ("newtheorem" "{[{[")
;;           ("providecommand" "*|{\\[[{")
;;           ("newcounter" "{[") ("renewenvironment" "*{[[{{")
;;           ("renewcommand" "*|{\\[[{") ("renewtheorem" "{[{[")
;;           ("usepackage" "[{[") ("fbox" "{") ("mbox" "{") ("rule" "[{{")
;;           ("vspace" "*{") ("hspace" "*{") ("thinspace" "") ("negthinspace" "")
;;           ;; XXX: Should macros without arguments rather be listed in a
;;           ;; separate category with 'noarg instead of 'command handling?
;;           ("enspace" "") ("enskip" "") ("quad" "") ("qquad" "") ("nonumber" "")
;;           ("centering" "") ("TeX" "") ("LaTeX" ""))
;;          'font-lock-function-name-face 2 command)
;;         ("sectioning-0"
;;          (("part" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-0-face)
;;          2 command)
;;         ("sectioning-1"
;;          (("chapter" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-1-face)
;;          2 command)
;;         ("sectioning-2"
;;          (("section" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-2-face)
;;          2 command)
;;         ("sectioning-3"
;;          (("subsection" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-3-face)
;;          2 command)
;;         ("sectioning-4"
;;          (("subsubsection" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-4-face)
;;          2 command)
;;         ("sectioning-5"
;;          (("paragraph" "*[{") ("subparagraph" "*[{")
;;           ("subsubparagraph" "*[{"))
;;          (if (eq font-latex-fontify-sectioning 'color)
;;              'font-lock-type-face
;;            'font-latex-sectioning-5-face)
;;          2 command)
;;         ("slide-title" () 'font-latex-slide-title-face 2 command)
;;         ("textual"
;;          (("item" "[") ("title" "{") ("author" "{") ("date" "{")
;;           ("thanks" "{") ("address" "{") ("caption" "[{")
;;           ("textsuperscript" "{"))
;;          'font-lock-type-face 2 command)
;;         ("bold-command"
;;          (("textbf" "{") ("textsc" "{") ("textup" "{") ("boldsymbol" "{")
;;           ("pmb" "{"))
;;          'font-latex-bold-face 1 command)
;;         ("italic-command"
;;          (("emph" "{") ("textit" "{") ("textsl" "{"))
;;          'font-latex-italic-face 1 command)
;;         ("math-command"
;;          (("ensuremath" "|{\\"))
;;          'font-latex-math-face 1 command)
;;         ("type-command"
;;          (("texttt" "{") ("textsf" "{") ("textrm" "{") ("textmd" "{"))
;;          'font-lock-type-face 1 command)
;;         ("bold-declaration"
;;          ("bf" "bfseries" "sc" "scshape" "upshape")
;;          'font-latex-bold-face 1 declaration)
;;         ("italic-declaration"
;;          ("em" "it" "itshape" "sl" "slshape")
;;          'font-latex-italic-face 1 declaration)
;;         ("type-declaration"
;;          ("tt" "ttfamily" "sf" "sffamily" "rm" "rmfamily" "mdseries"
;;           "tiny" "scriptsize" "footnotesize" "small" "normalsize"
;;           "large" "Large" "LARGE" "huge" "Huge")
;;          'font-lock-type-face 1 declaration)))
