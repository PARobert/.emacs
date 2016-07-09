<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"><head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<title>octave-hlp.el</title>
<style type="text/css">
.enscript-comment { font-style: italic; color: rgb(178,34,34); }
.enscript-function-name { font-weight: bold; color: rgb(0,0,255); }
.enscript-variable-name { font-weight: bold; color: rgb(184,134,11); }
.enscript-keyword { font-weight: bold; color: rgb(160,32,240); }
.enscript-reference { font-weight: bold; color: rgb(95,158,160); }
.enscript-string { font-weight: bold; color: rgb(188,143,143); }
.enscript-builtin { font-weight: bold; color: rgb(218,112,214); }
.enscript-type { font-weight: bold; color: rgb(34,139,34); }
.enscript-highlight { text-decoration: underline; color: 0; }
</style>
</head>
<body id="top">
<h1 style="margin:8px;" id="f1">octave-hlp.el&nbsp;&nbsp;&nbsp;<span style="font-weight: normal; font-size: 0.5em;">[<a href="http://opensource.apple.com//source/emacs/emacs-51/emacs/lisp/progmodes/octave-hlp.el?txt">plain text</a>]</span></h1>
<hr>
<div></div>
<pre><span class="enscript-comment">;;; octave-hlp.el --- getting help on Octave symbols using info
</span>
<span class="enscript-comment">;; Copyright (C) 1997 Free Software Foundation, Inc.
</span>
<span class="enscript-comment">;; Author: Kurt Hornik &lt;<a href="mailto:Kurt.Hornik@ci.tuwien.ac.at">Kurt.Hornik@ci.tuwien.ac.at</a>&gt;
</span><span class="enscript-comment">;; Author: John Eaton &lt;<a href="mailto:jwe@bevo.che.wisc.edu">jwe@bevo.che.wisc.edu</a>&gt;
</span><span class="enscript-comment">;; Maintainer: Kurt Hornik &lt;<a href="mailto:Kurt.Hornik@ci.tuwien.ac.at">Kurt.Hornik@ci.tuwien.ac.at</a>&gt;
</span><span class="enscript-comment">;; Keywords: languages
</span>
<span class="enscript-comment">;; This file is part of GNU Emacs.
</span>
<span class="enscript-comment">;; GNU Emacs is free software; you can redistribute it and/or modify
</span><span class="enscript-comment">;; it under the terms of the GNU General Public License as published by
</span><span class="enscript-comment">;; the Free Software Foundation; either version 2, or (at your option)
</span><span class="enscript-comment">;; any later version.
</span>
<span class="enscript-comment">;; GNU Emacs is distributed in the hope that it will be useful,
</span><span class="enscript-comment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of
</span><span class="enscript-comment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
</span><span class="enscript-comment">;; GNU General Public License for more details.
</span>
<span class="enscript-comment">;; You should have received a copy of the GNU General Public License
</span><span class="enscript-comment">;; along with GNU Emacs; see the file COPYING.  If not, write to the
</span><span class="enscript-comment">;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
</span><span class="enscript-comment">;; Boston, MA 02111-1307, USA.
</span>
<span class="enscript-comment">;;; Commentary:
</span>
<span class="enscript-comment">;; Provides the command `octave-help' which allows index lookup of a
</span><span class="enscript-comment">;; symbol in the Octave-related info files, as specified by the list
</span><span class="enscript-comment">;; `octave-help-files'.
</span>
<span class="enscript-comment">;; Other features may be added in future versions.
</span>
<span class="enscript-comment">;;; Code:
</span>
(require 'octave-mod)
(require 'info)

(defvar octave-help-files '(<span class="enscript-string">"octave"</span>)
  <span class="enscript-string">"List of info files with documentation for Octave.
Default is (\"octave\")."</span>)

(defvar octave-help-lookup-alist nil
  <span class="enscript-string">"Alist of Octave index entries for lookup."</span>)

(defvar octave-help-completion-alist nil
  <span class="enscript-string">"Alist of Octave index entries for completion.
The entries are of the form (VAR . VAR), where VAR runs through all
different keys in `octave-help-lookup-alist'."</span>)

<span class="enscript-comment">;;;###autoload
</span>(<span class="enscript-keyword">defun</span> <span class="enscript-function-name">octave-help</span> (key)
  <span class="enscript-string">"Get help on Octave symbols from the Octave info files.
Look up KEY in the function, operator and variable indices of the files
specified by `octave-help-files'.
If KEY is not a string, prompt for it with completion."</span>
  (interactive
   (list
    (completing-read (format <span class="enscript-string">"Describe Octave symbol: "</span>)
		     (octave-help-get-completion-alist)
		     nil t)))
  (<span class="enscript-keyword">if</span> (get-buffer <span class="enscript-string">"*info*"</span>)
      (set-buffer <span class="enscript-string">"*info*"</span>))
  (<span class="enscript-keyword">if</span> (zerop (length key))
      (Info-find-node (car octave-help-files) <span class="enscript-string">"Top"</span>)
    (<span class="enscript-keyword">let</span> ((alist (copy-alist (octave-help-get-lookup-alist)))
	  entry matches)
      (<span class="enscript-keyword">while</span> (setq entry (car alist))
	(<span class="enscript-keyword">if</span> (string-match key (car entry))
	    (add-to-list 'matches entry))
	(setq alist (cdr alist)))
      (<span class="enscript-keyword">if</span> matches
	  (<span class="enscript-keyword">progn</span>
	    (setq Info-index-alternatives matches)
	    (Info-index-next 0))))))

(<span class="enscript-keyword">defun</span> <span class="enscript-function-name">octave-help-get-lookup-alist</span> ()
  <span class="enscript-string">"Build the index lookup alist from all Octave info files.
The files specified by `octave-help-files' are searched."</span>
  (<span class="enscript-keyword">if</span> octave-help-lookup-alist
      ()
    (message <span class="enscript-string">"Building help lookup alist..."</span>)    
    (<span class="enscript-keyword">let</span> ((files octave-help-files) file key node)
      (<span class="enscript-keyword">save-window-excursion</span>
	(<span class="enscript-keyword">while</span> files
	  (setq file (car files))
 	  (Info-goto-node (concat <span class="enscript-string">"("</span> file <span class="enscript-string">")"</span>))
	  (<span class="enscript-keyword">condition-case</span> nil
	      (<span class="enscript-keyword">progn</span>
		(Info-index <span class="enscript-string">""</span>)
		(<span class="enscript-keyword">while</span>
		    (<span class="enscript-keyword">progn</span>
		      (<span class="enscript-keyword">while</span> (re-search-forward
			      <span class="enscript-string">"^\\* \\([^(:]+\\)[^:]*: *\\(.+\\)\\.$"</span>
			      nil t)
			(setq key (match-string 1)
			      node (concat <span class="enscript-string">"("</span> file <span class="enscript-string">")"</span> (match-string 2)))
			(<span class="enscript-keyword">and</span> (string-match <span class="enscript-string">"\\(.*\\&gt;\\) *$"</span> key)
			     (setq key (replace-match <span class="enscript-string">"\\1"</span> t nil key)))
			(add-to-list 'octave-help-lookup-alist
				     (list key
					   node
					   (concat (concat <span class="enscript-string">"("</span> file <span class="enscript-string">")"</span>)
						   Info-current-node)
					   0)))
		      (<span class="enscript-keyword">and</span> (setq node (Info-extract-pointer <span class="enscript-string">"next"</span> t))
			   (string-match
			    (concat <span class="enscript-string">"\\(Function\\|Operator\\|Variable\\) "</span>
				    <span class="enscript-string">"\\&lt;Index\\&gt;"</span>)
			    node)))
		  (Info-goto-node node)))
	    (error nil))
	  (setq files (cdr files)))))
    (message <span class="enscript-string">"Building help lookup alist...done"</span>))
  octave-help-lookup-alist)

(<span class="enscript-keyword">defun</span> <span class="enscript-function-name">octave-help-get-completion-alist</span> ()
  <span class="enscript-string">"Build the index completion alist from all Octave info files.
The files specified by `octave-help-files' are searched."</span>
  (<span class="enscript-keyword">if</span> octave-help-completion-alist
      ()
    (message <span class="enscript-string">"Building help completion alist..."</span>)
    (<span class="enscript-keyword">let</span> ((alist (octave-help-get-lookup-alist)) entry)
      (<span class="enscript-keyword">while</span> alist
	(setq entry (car alist))
	(add-to-list 'octave-help-completion-alist
		     (cons (car entry) (car entry)))
	(setq alist (cdr alist))))
    (message <span class="enscript-string">"Building help completion alist...done"</span>))    
  octave-help-completion-alist)

<span class="enscript-comment">;;; provide ourself
</span>
(provide 'octave-hlp)

<span class="enscript-comment">;;; octave-hlp.el ends here
</span></pre>
<hr>
</body></html>