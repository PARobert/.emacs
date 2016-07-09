;;; mwe-log-commands-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mwe-log-commands" "mwe-log-commands.el" (22304
;;;;;;  33573 902309 165000))
;;; Generated autoloads from mwe-log-commands.el

(autoload 'mwe:log-keyboard-commands "mwe-log-commands" "\
Enables keyboard command logging for the current buffer.
If optional ARG is nil, logging is turned off.

\(fn &optional ARG)" t nil)

(autoload 'mwe:open-command-log-buffer "mwe-log-commands" "\
Opens (and creates, if non-existant) a buffer used for logging keyboard commands.
If ARG is Non-nil, the existing command log buffer is cleared.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("mwe-log-commands-pkg.el") (22304 33573
;;;;;;  964818 573000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mwe-log-commands-autoloads.el ends here
