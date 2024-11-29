;;; nice-org-html.el --- prettier org-to-html export. -*- lexical-binding: t -*-
;;==============================================================================
;; Copyright (C) 2024, Ewan Townshend

;; Author: Ewan Townshend <ewan@etown.dev>
;; URL: https://github.com/ewantown/nice-org-html
;; Version: 1.0

;;==============================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;==============================================================================
;;; Commentary:

;; This provides an org-to-html publishing pipeline with emacs theme injection.
;; It enables exporting org files to readable, interactive, responsive html/css.
;; CSS colors are derived from specified light- and dark-mode emacs themes.
;; Layout is optimized for browser consumption of org files with toc and code.
;; Variables are defined to allow users to insert their own header/footer html.

;;; Credits:

;; Shi Tianshu's org-html-themify provided the basic model for css injection.
;; This package has diverged substantially, so is provided independently.

;; Various stackoverflow posts greatly helped, but alas, they are lost to me.

;;==============================================================================
;;; TODO:
;;
;; * Make function to "guess" face-attribute values that are undefined in theme

;;==============================================================================
;;; Code

;; Included in emacs >= 25.1
(require 'org)
(require 'ox)
(require 'ox-html)
(require 'ox-publish)

;; Other required
(require 's)
(require 'dash)
(require 'htmlize)
(require 'uuidgen)
(require 'hexrgb)

;;==============================================================================
;;; User configuration variables

;; Mandatory, with defaults:
(defvar nice-org-html-theme-alist '((light . tsdh-light) (dark . tsdh-dark))
  "Emacs themes used to generate inline css for 'light and 'dark modes.")

(defvar nice-org-html-default-mode 'dark
  "Default nice HTML page view-mode ('light or 'dark)")

;; Optional
(defvar nice-org-html-header ""
  "Path to (optional) header html file to inject as page header")
(defvar nice-org-html-footer ""
  "Path to (optional) footer html file to inject as page footer")
(defvar nice-org-html-css ""
  "Path to (optional) CSS file to inject")
(defvar nice-org-html-js ""
  "Path to (optional) JS  file to inject")

;;==============================================================================
;;; Package local variables

;; Backups of initial values
(defvar nice-org-html--initial-face-overrides nil)
(defvar nice-org-html--initial-default-style nil)
(defvar nice-org-html--initial-head-extra "")
(defvar nice-org-html--initial-preamble nil)
(defvar nice-org-html--initial-postamble nil)

;; Indicator to avoid overwriting initial values
(defvar nice-org-html--is-active nil)

;; Temp var to avoid needless reloading of themes
(defvar nice-org-html--temp-theme nil)

(defun nice-org-html--local-path (filename)
  "Get expanded path to a file local to this package"
  (let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name filename dir)))

(defvar nice-org-html--base-css (nice-org-html--local-path "nice-org-html.css")
  "Path to included CSS template that styles package-generated HTML")
(defvar nice-org-html--base-js  (nice-org-html--local-path "nice-org-html.js")
  "Path to included JS file that governs package-generated HTML")

;;==============================================================================
;; Setup and Teardown

(defun nice-org-html--setup ()
  (unless nice-org-html--is-active
    (add-hook 'org-export-before-processing-hook #'nice-org-html--inject)
    (setq nice-org-html--is-active t
	  org-html-head-include-default-style nil    
	  nice-org-html--initial-face-overrides htmlize-face-overrides
	  nice-org-html--initial-head-extra org-html-head-extra
	  nice-org-html--initial-preamble   'org-html-preamble
	  nice-org-html--initial-postamble  'org-html-postamble
	  htmlize-face-overrides
	  (append
	   nice-org-html--initial-face-overrides
	   '(font-lock-keyword-face
	     (:foreground "var(--clr-keyword)"
			  :background "var(--bg-keyword)")
	     font-lock-constant-face
	     (:foreground "var(--clr-constant)"
			  :background "var(--bg-constant)")
	     font-lock-comment-face
	     (:foreground "var(--clr-comment)"
			  :background "var(--bg-comment)")
	     font-lock-comment-delimiter-face
	     (:foreground "var(--clr-comment-delimiter)"
			  :background "var(--bg-comment-delimiter)")
	     font-lock-function-name-face
	     (:foreground "var(--function-clr-name)"
			  :background "var(--function-bg-name)")
	     font-lock-variable-name-face
	     (:foreground "var(--clr-variable)"
			  :background "var(--bg-variable)")
	     font-lock-preprocessor-face
	     (:foreground "var(--clr-preprocessor)"
			  :background "var(--bg-preprocessor)")
	     font-lock-doc-face
	     (:foreground "var(--clr-doc)"
			  :background "var(--bg-doc)")
	     font-lock-builtin-face
	     (:foreground "var(--clr-builtin)"
			  :background "var(--bg-builtin)")
	     font-lock-string-face
	     (:foreground "var(--clr-string)"
			  :background "var(--bg-string)"))))))

(defun nice-org-html--teardown ()
  (when nice-org-html--is-active
    (remove-hook 'org-export-before-processing-hook #'nice-org-html--inject)
    (setq nice-org-html--is-active nil
	  org-html-head-include-default-style nice-org-html--initial-default-style	      
	  htmlize-face-overrides nice-org-html--initial-face-overrides
	  org-html-head-extra nice-org-html--initial-head-extra
	  org-html-preamble   nice-org-html--initial-preamble
	  org-html-postamble  nice-org-html--initial-postamble)))

;;==============================================================================
;; HTML Modifications

(defun nice-org-html--inject (export-backend)
  "Inject page-level styling and scripts in header, preamble and postamble"
  (when (eq export-backend 'nice-html)
    (let ((style (nice-org-html--style))
	  (preamble (nice-org-html--preamble))
	  (postamble (nice-org-html--postamble)))
      (setq org-html-head-extra (concat style nice-org-html--initial-head-extra))
      (setq org-html-preamble   preamble)
      (setq org-html-postamble  postamble))))

(defun nice-org-html--style ()
  "Constructs html <style> element for header"
  (concat
   "<style type='text/css'>\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents nice-org-html--base-css)
     (when (and (not (equal "" nice-org-html-css))
		(file-exists-p nice-org-html-css))
       (insert-file-contents nice-org-html-css))
     (nice-org-html--interpolate-css)
     (buffer-string))
   "/*]]>*/-->\n"
   "</style>\n"))

(defun nice-org-html--preamble ()
  "Constructs html preamble to main content area"
  (concat
   (with-temp-buffer
     (when (and (not (equal "" nice-org-html-header))
		(file-exists-p nice-org-html-header))
       (insert "<div id='injected-header' class='injected'>")
       (insert (with-temp-buffer (insert-file-contents nice-org-html-header)
				 (buffer-string)))
       (insert "</div>"))
     (buffer-string))
   "<div id='view-controls'>"
   "<div id='toggle-mode'>&#9788;</div>"
   "<div id='toggle-toc'>&#9776;</div>"
   "</div>"))

(defun nice-org-html--postamble ()
  "Constructs html postamble to main content area"
  (concat
   (with-temp-buffer
     (when (and (not (equal "" nice-org-html-footer))
		(file-exists-p nice-org-html-footer))
       (insert "<div id='injected-footer' class='injected'>")
       (insert (with-temp-buffer (insert-file-contents nice-org-html-footer)
				 (buffer-string)))
       (insert "</div>"))
     (buffer-string))
   "<script type=\"text/javascript\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   "document.cookie = 'theme-mode="
   (if (eq nice-org-html-default-mode 'light) "light" "dark") "'\n"
   (with-temp-buffer
     (insert-file-contents nice-org-html--base-js)
     (when (and (not (equal "" nice-org-html-js))
		(file-exists-p nice-org-html-js))
       (insert-file-contents nice-org-html-js))
     (buffer-string))
   "/*]]>*/-->\n"
   "</script>"
   "<div hidden>"
   "Generated by: https://github.com/ewantown/nice-org-html"   
   "</div>"))

;;==============================================================================
;; Emacs theme / CSS Interpolation

(defun nice-org-html--interpolate-css ()
  "Interpolate hex values in CSS template"
  (let ((initial-themes custom-enabled-themes))
    (setq inhibit-redisplay t)
    (mapc (lambda (th) (disable-theme th)) initial-themes)
    (goto-char (point-min))
    ;; loop over CSS template variables
    (while (re-search-forward "#{.+?}" nil t)
      (-let* ((beg (match-beginning 0))
	      (end (match-end 0))
	      (str (buffer-substring-no-properties beg end))
	      (val (nice-org-html--get-hex-val str)))
	(delete-region beg end)
	(insert val)))
    ;; restore prior theme configuration
    (unless (-contains? initial-themes nice-org-html--temp-theme)
      (disable-theme nice-org-html--temp-theme)
      (setq custom-enabled-themes initial-themes))
    (mapc (lambda (th) (load-theme th t nil)) initial-themes)
    (setq inhibit-redisplay nil)))

(defun nice-org-html--get-hex-val (str)
  "Parse/Interp string of form #{mode:entity:attribute:key?|...} against themes"
  (let* ((clauses (split-string (substring str 2 -1) "|"))
	 (val (car (-keep 'nice-org-html--interp-clause clauses))))
    (cond ((null val) "initial")
	  ((hexrgb-rgb-hex-string-p val) val)
	  ((hexrgb-color-name-to-hex val 2)))))

(defun nice-org-html--interp-clause (c)
  (-let* (((m  e  a  k)  (s-split ":" c))
	  ((ms es as ks) `(,(intern m)
			   ,(intern e)
			   ,(intern (concat ":" a))
			   ,(and k (intern (concat ":" k)))))
	  (theme (alist-get ms nice-org-html-theme-alist)))
    (progn
      ;; load theme associated with mode of the clause
      (unless (equal theme nice-org-html--temp-theme)
	(disable-theme nice-org-html--temp-theme)
	(load-theme theme t nil)
	(setq nice-org-html--temp-theme theme))
      ;; grab value for face-attribute specified by clause
      (let ((val (face-attribute es as)))
	(unless (or (not val) (equal val 'unspecified))
	  (if ks (plist-get val ks) val))))))

;;==============================================================================
;; Custom export backend for better org doc content export

(defun nice-org-html--src-block (src-block contents info)
  "Transform org-src-block to html, adding a 'copy to clipboard' button."
    (let* ((btn-id (concat "btn_" (s-replace "-" "" (uuidgen-4))))
	   (content
	    (let ((print-escape-newlines t))
	      (prin1-to-string (org-export-format-code-default src-block info))))
	   (content^
	    (s-chop-prefix "\""
			   (s-chop-suffix "\""
					  (s-replace "`" "\\`" content)))))
      (concat "<div class='org-src-wrapper'>\n"
	      (org-export-with-backend 'html src-block contents info)
	      (nice-org-html--copy-src-button btn-id)
	      (nice-org-html--copy-src-script btn-id content^)
	      "</div>")))

(defun nice-org-html--copy-src-button (btn-id)
  (concat "<button class='copyBtn' name=" btn-id ">copy</button>"))

(defun nice-org-html--copy-src-script (btn-id txt)
  (concat "\n<script type='text/javascript'>\n"
	  "var copyBtn" btn-id "=document.querySelector('button[name=" btn-id "]');\n"
	  "copyBtn" btn-id ".addEventListener('click', function(event) {\n"
	  "let res = copyTextToClipboard(`" txt "`);"
	  "copyBtn" btn-id ".textContent = res ? 'copied' : 'error';"
	  "setTimeout(() => { copyBtn" btn-id ".textContent = 'copy';}, 3000);"
	  "\n});\n</script>\n"))

(org-export-define-derived-backend 'nice-html 'html
  :translate-alist '((src-block . nice-org-html--src-block))
  :menu-entry
  '(?H "Export to nice HTML"
       ((?h "As HTML file" nice-org-html-export-to-html)
	(?H "As specified file" nice-org-html-export-to-html-file)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (nice-org-html-export-to-html t s v b)
		(org-open-file (nice-org-html-export-to-html nil s v b))))))))

;;==============================================================================
;; These functions extend the (similarly named) ox-html ones to the new backend

(defun nice-org-html-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to HTML file in PWD using nice-org-html custom backend.
   See docs for org-html-export-to-html, which this function emulates."
  (interactive)
  (let* ((nice-org-html-header (read-string "HTML header file (optional): "
					    nice-org-html-header nil nil nil))
	 (nice-org-html-footer (read-string "HTML footer file (optional): "
					    nice-org-html-footer nil nil nil))
	 (nice-org-html-css (read-string "Additional CSS file (optional): "
					 nice-org-html-css nil nil nil))
	 (nice-org-html-js (read-string "Additional JS file (optional): "
					nice-org-html-js nil nil nil))
	 (extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'nice-html file
      async subtreep visible-only body-only ext-plist)))

(defun nice-org-html-export-to-html-file
    (&optional async subtreep visible-only body-only ext-plist)
  "Exports current buffer as nice HTML to interactively specified file"
  (let* ((file (read-string "Target file path (mandatory): "))
	 (nice-org-html-header (read-string "HTML header file (optional): "
					    nice-org-html-header nil nil nil))
	 (nice-org-html-footer (read-string "HTML footer file (optional): "
					    nice-org-html-footer nil nil nil))
	 (nice-org-html-css (read-string "Additional CSS file (optional): "
					 nice-org-html-css nil nil nil))
	 (nice-org-html-js (read-string "Additional JS file (optional): "
					nice-org-html-js nil nil nil)))
    (org-export-to-file 'nice-html file
      async subtreep visible-only body-only ext-plist nil)))

(defun nice-org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML using nice-org-html custom export backend.
   See docs for org-html-publish-to-html, which this function emulates."
  (org-publish-org-to 'nice-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

(defmacro nice-org-html-make-publish-to-html-with
    (&optional theme-alist default-mode header-html footer-html css js)
  "Creates org-publishing function which quasi-closes over passed configuration"
  (let ((sym (gensym)))
    `(progn
       (defun ,sym (plist filename pub-dir)       
	 (let* ((theme-alist (or (and (listp ,theme-alist)
				      (assoc 'light ,theme-alist)
				      (assoc 'dark  ,theme-alist)
				      ,theme-alist)
				 nice-org-html-theme-alist))
		(default-mode (or (and (memq ,default-mode '(light dark))
				       ,default-mode)
				  nice-org-html-default-mode))
		(nice-org-html-theme-alist  theme-alist)
		(nice-org-html-default-mode default-mode)
		(nice-org-html-header ,header-html)
		(nice-org-html-footer ,footer-html)
		(nice-org-html-css ,css)
		(nice-org-html-js  ,js))
	   (nice-org-html-publish-to-html plist filename pub-dir)))
       ',sym)))


;;==============================================================================
;; Defined mode

(define-minor-mode nice-org-html-mode
  "Mode for prettier export of .org to .html"
  :version 1.0
  (if nice-org-html-mode (nice-org-html--setup) (nice-org-html--teardown)))

;;==============================================================================
(provide 'nice-org-html)
