;; et-org-html.el -*- lexical-binding: t -*-
;;==============================================================================
;; Copyright (C) 2024, Ewan Townshend

;; Author: Ewan Townshend
;; URL: TBD
;; Version: 1.0

;;==============================================================================
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

;; This file is not part of emacs.

;;==============================================================================
;;; Commentary:

;; This provides an org-to-html publishing pipeline with emacs theme injection.
;; It enables exporting org files to readable, interactive, responsive html/css.
;; CSS colors are derived from specified light- and dark-mode emacs themes.
;; Layout is optimized for browser consumption of org files with toc and code.
;; Variables are defined to allow users to insert their own header/footer html.

;;; Credits:

;; Shi Tianshu's org-html-themify provided the basic model for css injection.
;; I have modified that package substantially, so am providing this separately.

;; Various stackoverflow posts greatly helped, but alas, they are lost to me.

;;==============================================================================
;; Package requires et-org-html.css, et-org-html.js, and:

(require 's)
(require 'dash)
(require 'org)
(require 'htmlize)
(require 'hexrgb)
(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'uuidgen)

;;==============================================================================
;; Variables

(defvar et-org-html-theme-alist '((dark . tango-dark) (light . leuven))
  "Emacs themes used to generate inline css for dark and light modes.")

(defun et-org-html-filepath (filename)
  "Get expanded path to a file local to this package"
  (let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name filename dir)))

;; These two are required for functioning of this package
(defvar et-org-html-css-path (et-org-html-filepath "et-org-html.css")
  "Path to (mandatory) CSS /template/ file for styling generated HTML")
(defvar et-org-html-js-path  (et-org-html-filepath "et-org-html.js")
  "Path to (mandatory) JS file that governs generated HTML")

;; These are all optional
(defvar et-org-html-header-path ""
  "Path to (optional) header html file to inject as site header")
(defvar et-org-html-footer-path ""
  "Path to (optional) footer html file to inject as site footer")
(defvar et-org-html-extra-css-path ""
  "Path to (optional) CSS file to inject")
(defvar et-org-html-extra-js-path ""
  "Path to (optional) JS  file to inject")

;; Backups of initial values
(defvar et-org-html-initial-face-overrides nil)
(defvar et-org-html-initial-default-style nil)
(defvar et-org-html-initial-head-extra "")
(defvar et-org-html-initial-preamble nil)
(defvar et-org-html-initial-postamble nil)

;; Indicator to avoid overwriting initial values
(defvar et-org-html-is-active nil)

;; Temp var to avoid needless reloading of themes
(defvar et-org-html-temp-theme nil)

;;==============================================================================
;; Setup and Teardown

(defun et-org-html-setup ()
  "Initialize theme-injecting org-to-html publishing pipeline"
  (unless et-org-html-is-active
    (setq et-org-html-is-active t)
    (setq org-html-head-include-default-style nil)
    (add-hook 'org-export-before-processing-hook 'et-org-html-inject)
    (setq et-org-html-initial-face-overrides htmlize-face-overrides
	  et-org-html-initial-head-extra org-html-head-extra
	  et-org-html-initial-preamble   'org-html-preamble
	  et-org-html-initial-postamble  'org-html-postamble)
    (setq htmlize-face-overrides
	  (append
	   et-org-html-initial-face-overrides
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
(et-org-html-setup)
(defun et-org-html-teardown ()
  (when et-org-html-is-active
    (setq et-org-html-is-active nil)
    (setq org-html-head-include-default-style et-org-html-initial-default-style)
    (remove-hook 'org-export-before-processing-hook 'et-org-html-inject-style)
    (setq htmlize-face-overrides et-org-html-initial-face-overrides
	  org-html-head-extra    et-org-html-initial-head-extra
	  org-html-preamble      et-org-html-initial-preamble
	  org-html-postamble     et-org-html-initial-postamble)))

;;==============================================================================
;; HTML Modifications

(defun et-org-html-inject (export-backend)
  "Inject page-level styling and scripts in header, preamble and postamble"
  (when (or (eq export-backend 'html) (eq export-backend 'et-html))
    (let ((style (et-org-html-style))
	  (preamble (et-org-html-preamble))
	  (postamble (et-org-html-postamble)))
      (setq org-html-head-extra (concat style et-org-html-initial-head-extra))
      (setq org-html-preamble   preamble)
      (setq org-html-postamble  postamble))))

(defun et-org-html-style ()
  "Constructs html <style> element for header"
  (concat
   "<style type='text/css'>\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents et-org-html-css-path)
     (when (and (not (equal "" et-org-html-extra-css-path))
		(file-exists-p et-org-html-extra-css-path))
       (insert-file-contents et-org-html-extra-css-path))
     (et-org-html-interpolate-css)
     (buffer-string))
   "/*]]>*/-->\n"
   "</style>\n"))

(defun et-org-html-preamble ()
  "Constructs html preamble to main content area"
  (concat
   (with-temp-buffer
     (when (and (not (equal "" et-org-html-header-path))
		(file-exists-p et-org-html-header-path))
       (insert "<div id='injected-head' class='injected'>")
       (insert (with-temp-buffer (insert-file-contents et-org-html-header-path)
				 (buffer-string)))
       (insert "</div>"))
     (buffer-string))
   "<div id='view-controls'>"
   "<div id='toggle-mode'>&#9788;</div>"
   "<div id='toggle-toc'>&#9776;</div>"
   "</div>"))

(defun et-org-html-postamble ()
  "Constructs html postamble to main content area"
  (concat
   (with-temp-buffer
     (when (and (not (equal "" et-org-html-footer-path))
		(file-exists-p et-org-html-footer-path))
       (insert "<div id='injected-foot' class='injected'>")
       (insert (with-temp-buffer (insert-file-contents et-org-html-footer-path)
				 (buffer-string)))
       (insert "</div>"))
     (buffer-string))
   "<script type=\"text/javascript\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents et-org-html-js-path)
     (when (and (not (equal "" et-org-html-extra-js-path))
		(file-exists-p et-org-html-extra-js-path))
       (insert-file-contents et-org-html-extra-js-path))
     (buffer-string))
   "/*]]>*/-->\n"
   "</script>"))

;;==============================================================================
;; Emacs theme / CSS Interpolation

(defun et-org-html-interpolate-css ()
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
	      (val (et-org-html-get-hex-val str)))
	(delete-region beg end)
	(insert val)))
    (unless (-contains? initial-themes et-org-html-temp-theme)
      (disable-theme et-org-html-temp-theme)
      (setq custom-enabled-themes initial-themes))
    (mapc (lambda (th) (load-theme th t nil)) initial-themes)
    ;;(load-theme (car initial-themes) t nil)
    (setq inhibit-redisplay nil)))

(defun et-org-html-get-hex-val (str)
  "Parse/Interp string form #{mode:entity:attribute:key?|...} against themes"
  (let* ((clauses (split-string (substring str 2 -1) "|"))
	 (val (car (-keep 'et-org-html-interp-clause clauses))))
    (cond ((null val) "initial") ;; TODO - function for best guess at null vals
	  ((hexrgb-rgb-hex-string-p val) val)
	  ((hexrgb-color-name-to-hex val 2)))))

(defun et-org-html-interp-clause (c)
  (-let* (((m  e  a  k)  (s-split ":" c))
	  ((ms es as ks) `(,(intern m)
			   ,(intern e)
			   ,(intern (concat ":" a))
			   ,(and k (intern (concat ":" k)))))
	  (theme (alist-get ms et-org-html-theme-alist)))
    (progn
      ;; load theme associated with mode of the clause
      (unless (equal theme et-org-html-temp-theme)
	(disable-theme et-org-html-temp-theme)
	(load-theme theme t nil)
	(setq et-org-html-temp-theme theme))
      ;; grab value for face-attribute specified by clause
      (let ((val (face-attribute es as)))
	(unless (or (not val) (equal val 'unspecified))
	  (if ks (plist-get val ks) val))))))

;;==============================================================================
;; Custom backend for better org doc content export

;; This enables a copy-to-clipboard button on org source blocks
(defun et-org-html-src-block (src-block contents info)
  "Transcode org-src-block to html, adding a 'copy to clipboard' button."
  (if (not (org-export-read-attribute :attr_html src-block :copy-button))
      (org-export-with-backend 'html src-block contents info)
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
	      (et-org-html-copy-src-button btn-id)
	      (et-org-html-copy-src-script btn-id content^)
	      "</div>"))))

(defun et-org-html-copy-src-button (btn-id)
  (concat "<button class='copyBtn' name=" btn-id ">copy</button>"))
(defun et-org-html-copy-src-script (btn-id txt)
  (concat "\n<script type='text/javascript'>\n"
	  "var copyBtn" btn-id "=document.querySelector('button[name=" btn-id "]');\n"
	  "copyBtn" btn-id ".addEventListener('click', function(event) {\n"
	  "copyTextToClipboard(`" txt "`);\n});\n</script>\n"))


(org-export-define-derived-backend 'et-html 'html
  :translate-alist '((src-block . et-org-html-src-block)))

(defun et-org-html-export-to-html (file)
  "Exports the current org-mode buffer to customized HTML file"
  (interactive "FFile Name: ")
  (org-export-to-file 'et-html file))

;;==============================================================================
;; This extends the ox-html function org-html-publish-to-html to the new backend
(defun et-org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML using a custom backend.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'et-html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))

;;==============================================================================
;; Defined mode

(define-minor-mode et-org-html-mode
  "Mode for prettier export of .org to .html"
  :version 1.0
  (if et-org-html-mode (et-org-html-setup) (et-org-html-teardown)))

;;==============================================================================
(provide 'et-org-html)
