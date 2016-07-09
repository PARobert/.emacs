;; --------------------------------------------------------------------------------
;; Filename : init-eviews-mode.el
;; Author : Pierre-Antoine ROBERT <pierre.antoine.ROBERT@ensae-paristech.fr>
;; 
;; Description : init file for eview mode. The initial file was written by Joe
;; Bloggs <vapniks@yahoo.com> and completed by me.
;; 
;; Copyleft (C) 2013, Joe Bloggs, all rites reversed.
;; --------------------------------------------------------------------------------

(defun eviews-mode ()
  "Major mode for editing eviews programs."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table eviews-mode-syntax-table)
  (use-local-map eviews-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(eviews-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) 'eviews-indent-line)
  (setq comment-start-skip "' ")
  (setq comment-start "' ")
  (setq major-mode 'eviews-mode)
  (setq mode-name "Eviews")
  (run-hooks 'eviews-mode-hook))

;; --------------------------------------------------------------------------------
;;     Variables
;; --------------------------------------------------------------------------------

(defvar eviews-mode-hook nil)
(defvar eviews-indent 4)

(defconst eviews-font-lock-keywords-1
  (list
   ;; control statements
   '("\\<\\(call\\|e\\(?:lse\\|nd\\(?:if\\|sub\\)\\|xitloop\\)\\|for\\|if\\|next\\|return\\|s\\(?:t\\(?:[eo]p\\)\\|ubroutine\\)\\|t\\(?:hen\\|o\\)\\|w\\(?:end\\|hile\\)\\)\\>" . font-lock-keyword-face)
   ;; object types
    '("\\<\\(alpha\\|coef\\|equation\\|f\\(?:actor\\|rml\\)\\|g\\(?:enr\\|r\\(?:aph\\|oup\\)\\)\\|l\\(?:ink\\|ogl\\)\\|m\\(?:atrix\\|odel\\)\\|pool\\|rowvector\\|s\\(?:ample\\|calar\\|eries\\|pool\\|space\\|y\\(?:\\(?:ste\\)?m\\)\\)\\|t\\(?:able\\|ext\\)\\|v\\(?:a\\(?:lmap\\|r\\)\\|ector\\)\\)\\>" . font-lock-type-face)
    ;; control and string variables (start with ! or %)
   '("\[%!\]\\w+" . font-lock-variable-name-face)
   ;; misc matrix functions
   '("\\<\\(colplace\\|m\\(?:atplace\\|tos\\)\\|rowplace\\|stom\\(?:na\\)?\\)\\>" . font-lock-function-name-face)
   ;; functions (start with @)
   '("\[@\]\\w+" . font-lock-function-name-face)
   ;; include statement
   '("\\<include\\>" . font-lock-preprocessor-face)
   ;; views for objects
   '("\\.\\(?:a\\(?:nticov\\|ppend\\|r\\(?:chtest\\|lm\\|ma\\|roots\\)\\|uto\\)\\|b\\(?:dstest\\|lock\\)\\|c\\(?:ause\\|ellipse\\|h\\(?:eckderivs\\|ow\\)\\|o\\(?:efcov\\|int\\|rrel\\(?:sq\\)?\\|[rv]\\)\\|ross\\)\\|d\\(?:e\\(?:comp\\|pfreq\\|rivs\\|scribe\\)\\|isplay\\|table\\)\\|e\\(?:dftest\\|igen\\|ndog\\|qs\\)\\|f\\(?:acbreak\\|i\\(?:t\\(?:stats\\|ted\\)\\|xedtest\\)\\|req\\)\\|g\\(?:arch\\|rads\\)\\|h\\(?:\\(?:ette\\|i\\)st\\)\\|impulse\\|jbera\\|l\\(?:a\\(?:bel\\|glen\\)\\|oadings\\)\\|m\\(?:axcor\\|eans\\|s[ag]\\)\\|o\\(?:bserved\\|utput\\)\\|p\\(?:artcor\\|comp\\|redict\\)\\|q\\(?:r\\(?:process\\|s\\(?:lope\\|ymm\\)\\)\\|stats\\)\\|r\\(?:anhaus\\|e\\(?:duced\\|presentations\\|s\\(?:et\\|id\\(?:co[rv]\\|s\\)\\|ults\\)\\)\\|ls\\|otateout\\)\\|s\\(?:cores\\|heet\\|ignalgraphs\\|mc\\|pec\\|t\\(?:at\\(?:by\\|e\\(?:final\\|graphs\\|init\\)\\|s\\)\\|ructure\\)\\)\\|t\\(?:able\\|e\\(?:st\\(?:add\\|b\\(?:tw\\|y\\)\\|drop\\|exog\\|fit\\|lags\\|stat\\)\\|xt\\)\\)\\|u\\(?:nbreak\\|root\\|sage\\)\\|vars\\|w\\(?:ald\\|hite\\)\\)" . font-lock-builtin-face)
   ;; graph views for objects
   '("\\.\\(?:area\\|b\\(?:a\\(?:nd\\|r\\)\\|oxplot\\)\\|d\\(?:\\(?:istpl\\)?ot\\)\\|errbar\\|hilo\\|line\\|pie\\|qqplot\\|s\\(?:cat\\(?:mat\\|pair\\)?\\|easplot\\|pike\\)\\|xy\\(?:area\\|bar\\|line\\|pair\\)\\)" . font-lock-builtin-face)
   ;; procs for objects
   '("\\.\\(a\\(?:dd\\(?:assign\\|\\(?:ini\\|tex\\)t\\)?\\|lign\\|ppend\\|xis\\)\\|bpf\\|c\\(?:l\\(?:assify\\|eartext\\)\\|o\\(?:mment\\|ntrol\\)\\)\\|d\\(?:atelabel\\|e\\(?:fine\\|lete\\(?:col\\|row\\)?\\)\\|is\\(?:playname\\|tdata\\)\\|r\\(?:aw\\(?:default\\)?\\|op\\)\\)\\|ex\\(?:clude\\|tract\\)\\|f\\(?:actnames\\|etch\\|i\\(?:ll\\|t\\)\\|latten\\|orecast\\)\\|g\\(?:enr\\|raphmode\\)\\|h\\(?:orizindent\\|pf\\)\\|in\\(?:nov\\|sert\\(?:col\\|row\\)?\\)\\|l\\(?:abel\\|e\\(?:ftmargin\\|gend\\)\\)\\|m\\(?:a\\(?:ke\\(?:coint\\|derivs\\|endog\\|filter\\|g\\(?:arch\\|r\\(?:a\\(?:ds\\|ph\\)\\|oup\\)\\)\\|l\\(?:imits\\|oglike\\)\\|m\\(?:ap\\|odel\\)\\|pcomp\\|re\\(?:\\(?:g\\|sid\\)s\\)\\|s\\(?:cores\\|ignals\\|tat\\(?:e?s\\)\\|ystem\\)\\)\\|p\\)\\|\\(?:erg\\|ov\\)e\\)\\|name\\|o\\(?:ptions\\|verride\\)\\|print\\|r\\(?:e\\(?:ad\\|\\(?:mov\\|sampl\\)e\\)\\|otate\\(?:clear\\)?\\)\\|s\\(?:ave\\|cenario\\|e\\(?:as\\|t\\(?:bpelem\\|convert\\|elem\\|f\\(?:illcolor\\|o\\(?:\\(?:n\\|rma\\)t\\)\\)\\|height\\|indent\\|just\\|lines\\|merge\\|obslabel\\|t\\(?:extcolor\\|race\\)\\|width\\)?\\)\\|mooth\\|o\\(?:lve\\(?:opt\\)?\\|rt\\)\\|pec\\|to\\(?:chastic\\|m\\(?:na\\)?\\|re\\)\\|var\\)\\|t\\(?:ablemode\\|e\\(?:mplate\\|xtdefault\\)\\|itle\\|opmargin\\|ra\\(?:c[ek]\\|moseats\\)\\)\\|u\\(?:nlink\\|pdate\\(?:coefs\\)?\\)\\|vert\\(?:indent\\|spacing\\)\\|w\\(?:idth\\|rite\\)\\|x1[12]\\)" . font-lock-builtin-face)
   ;; methods for objects
   '("\\.\\(m1\\|3sls\\|Is\\|arch\\|binary\\|c\\(?:ensored\\|ount\\)\\|ec\\|fiml\\|g\\(?:ls\\|mm\\)\\|ipf\\|logit\\|ml\\|ordered\\|p\\(?:ace\\|f\\|robit\\)\\|qreg\\|s\\(?:tepls\\|ur\\)\\|\\(?:ts\\|wts\\|[uw]\\)ls\\)" . font-lock-builtin-face)
   ;; general commands
   '("\\<\\(Is\\|a\\(?:r\\(?:ch\\(?:test\\)?\\|ea\\)\\|uto\\)\\|b\\(?:a\\(?:nd\\|r\\)\\|inary\\|oxplot\\)\\|c\\(?:ause\\|copy\\|d\\|ensored\\|fetch\\|how\\|l\\(?:abel\\|ose\\)\\|o\\(?:int\\|py\\|unt\\|[rv]\\)\\|ross\\)\\|d\\(?:ata\\|b\\(?:c\\(?:opy\\|reate\\)\\|delete\\|open\\|pack\\|re\\(?:build\\|name\\|pair\\)\\)\\|elete\\|\\(?:istplo\\|o\\|riconver\\)t\\|[bo]\\)\\|e\\(?:rrbar\\|x\\(?:it\\|pand\\)\\)\\|f\\(?:ac\\(?:break\\|test\\)\\|etch\\|it\\|orecast\\|r\\(?:eeze\\|ml\\)\\)\\|g\\(?:enr\\|mm\\)\\|h\\(?:convert\\|fetch\\|i\\(?:lo\\|st\\)\\|label\\|pf\\)\\|l\\(?:ine\\|o\\(?:ad\\|git\\)\\)\\|o\\(?:pen\\|rdered\\|utput\\)\\|p\\(?:a\\(?:ge\\(?:append\\|c\\(?:o\\(?:ntract\\|py\\)\\|reate\\)\\|delete\\|load\\|re\\(?:fresh\\|name\\)\\|s\\(?:ave\\|elect\\|t\\(?:ack\\|ruct\\)\\)\\|un\\(?:\\(?:lin\\|stac\\)k\\)\\)\\|ram\\)\\|ie\\|o\\(?:ff\\|n\\)\\|r\\(?:int\\|o\\(?:bit\\|gram\\)\\)\\)\\|q\\(?:qplot\\|reg\\)\\|r\\(?:ange\\|e\\(?:ad\\|name\\|set\\)\\|nd\\(?:int\\|seed\\)\\|un\\)\\|s\\(?:ave\\|cat\\(?:mat\\|pair\\)?\\|e\\(?:as\\(?:plot\\)?\\|t\\(?:c\\(?:ell\\|olwidth\\)\\|line\\)\\)\\|how\\|m\\(?:ooth\\|pl\\)\\|o\\(?:lve\\|rt\\)\\|p\\(?:awn\\|ike\\)\\|t\\(?:at\\(?:s\\|usline\\)\\|epls\\|ore\\)\\)\\|t\\(?:est\\(?:add\\|drop\\)\\|ic\\|oc\\|sls\\)\\|u\\(?:n\\(?:\\(?:brea\\|lin\\)k\\)\\|root\\)\\|varest\\|w\\(?:f\\(?:create\\|open\\|refresh\\|s\\(?:ave\\|elect\\|tats\\)\\|unlink\\)\\|\\(?:orkfil\\|rit\\)e\\)\\|xy\\(?:area\\|bar\\|line\\|pair\\)\\)\\>" . font-lock-builtin-face))
   "Minimal highlighting expressions for eviews mode.")

(defvar eviews-font-lock-keywords eviews-font-lock-keywords-1
  "Default highlighting expressions for eviews mode.")

(defvar eviews-mode-syntax-table
  (let ((eviews-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?' "< " eviews-mode-syntax-table)
    (modify-syntax-entry ?\n "> " eviews-mode-syntax-table)
    (modify-syntax-entry ?\\ ". " eviews-mode-syntax-table)
    eviews-mode-syntax-table)
  "Syntax table for eviews-mode.")

;; --------------------------------------------------------------------------------
;;     Functions
;; --------------------------------------------------------------------------------

(defun eviews-indent-line ()
  "Indent current line as eviews code."
  (interactive)
  (beginning-of-line)
  (if (bobp) 
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(else\\|endif\\|next\\|wend\\|endsub\\)")
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) eviews-indent)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "^[ \t]*\\(endif\\|next\\|wend\\|endsub\\)")
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "^[ \t]*\\(if\\|else\\|for\\|while\\|subroutine\\)")
		  (progn
		    (setq cur-indent (+ (current-indentation) eviews-indent))
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

;; --------------------------------------------------------------------------------
;;     Key map
;; --------------------------------------------------------------------------------

(defvar eviews-mode-map
  (let ((eviews-mode-map (make-keymap)))
    (define-key eviews-mode-map "\C-j" 'newline-and-indent)
    eviews-mode-map)
  "Keymap for eviews major mode.")


;; --------------------------------------------------------------------------------

(provide 'init-eviews-mode)
