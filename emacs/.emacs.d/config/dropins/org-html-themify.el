;;; org-html-themify.el --- Themify org-mode export with Emacs color theme
;;; -*- lexical-binding: t -*-

;; Original Author: Shi Tianshu
;; Modified By: Ewan Townshend
;; Keywords: org-mode
;; Package-Requires: ((emacs "27.1") (org "9.4.4") (htmlize "1.5.6") (dash "2.17.0") (hexrgb "0") (s "1.12.0"))
;; Version: 2.0
;; URL: TODO
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 1. Specify a light and a dark theme.
;;
;; (setq org-html-themify-themes '((dark . solarized-dark) (light . solarized-light)))
;;
;; 2. Enable org-html-themify-themes in org-mode.
;;
;; (add-hook 'org-mode-hook #'org-html-themify-mode)
;;
;; That's all, now you can export HTML.

;;; Code:

(require 'hexrgb)
(require 'htmlize)
(require 'dash)
(require 'org)
(require 's)

(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(use-package uuidgen)

(defvar org-html-themify-themes
  '((dark . tango-dark)
    (light . leuven))
  "Themes used to generate inline stylesheet.")

(defvar org-html-themify--current-theme nil)

(defvar org-html-themify--backup-htmlize-face-overrides nil)

(defvar org-html-themify--backup-org-html-preamble nil)

(defvar org-html-themify--backup-org-html-head-extra "")

(defvar org-html-themify--backup-org-html-postamble nil)

(defvar org-html-themify-css-path
  (expand-file-name
   "org-html-themify.css"
   (file-name-directory (or load-file-name (buffer-file-name)))))

(defvar org-html-themify-js-path
  (expand-file-name
   "org-html-themify.js"
   (file-name-directory (or load-file-name (buffer-file-name)))))

(defun org-html-themify--parse-clause (clause)
  (-let* (((th f a k) (s-split ":" clause))
          (fs (intern f))
          (as (intern (concat ":" a)))
          (req-theme (alist-get (intern th) org-html-themify-themes))
          (_ (unless (equal req-theme org-html-themify--current-theme)
               (when org-html-themify--current-theme
		 (disable-theme org-html-themify--current-theme))
               (load-theme req-theme t)
               (setq org-html-themify--current-theme req-theme))))
    (let ((v (and (not (equal fs 'hl-line)) (face-attribute fs as))))
      (unless (or (not v) (equal v 'unspecified))
        (if k
            (plist-get v (intern (concat ":" k)))
          v)))))

(defun org-html-themify--get-interpolate-value (s)
  (let* ((clauses (-> s
                      (substring 2 -1)
                      (split-string "|")))
         (vals (-keep #'org-html-themify--parse-clause clauses))
         (val (car vals)))
    (cond
     ((null val) "initial")
     ((hexrgb-rgb-hex-string-p val) val)
     ((hexrgb-color-name-to-hex val 2)))))

(defun org-html-themify--interpolate ()
  (let ((inhibit-redisplay t)
        (orig-themes custom-enabled-themes))
    (mapc (lambda (th) (disable-theme th)) orig-themes)
    (goto-char (point-min))
    (while (re-search-forward "#{.+?}" nil t)
      (-let* ((beg (match-beginning 0))
              (end (match-end 0))
              (s (buffer-substring-no-properties beg end))
              (v (org-html-themify--get-interpolate-value s)))
        (delete-region beg end)
        (insert v)))
    (disable-theme org-html-themify--current-theme)
    (mapc (lambda (th) (load-theme th t)) orig-themes)))

(defun org-html-themify--setup-inlines (exporter)
  "Insert custom inline css"
  (when (or (eq exporter 'html) (eq exporter 'et-html))
    (setq org-html-preamble
          (concat
           "<div id=\"toggle-theme\">&#9788;</div>"
           "<div id=\"toggle-toc\">&#9776;</div>"))
    (setq org-html-head-extra
          (concat
           org-html-themify--backup-org-html-head-extra
           "<style type=\"text/css\">\n"
           "<!--/*--><![CDATA[/*><!--*/\n"
           (with-temp-buffer
             (insert-file-contents org-html-themify-css-path)
             (org-html-themify--interpolate)
             (buffer-string))
           "/*]]>*/-->\n"
           "</style>\n"))
    (setq org-html-postamble
          (concat
           "<script type=\"text/javascript\">\n"
           "<!--/*--><![CDATA[/*><!--*/\n"
           (with-temp-buffer
             (insert-file-contents org-html-themify-js-path)
             (buffer-string))
           "/*]]>*/-->\n"
           "</script>"))))

(defun org-html-themify--init ()
  (add-hook 'org-export-before-processing-hook 'org-html-themify--setup-inlines)
  (setq org-html-themify--backup-htmlize-face-overrides htmlize-face-overrides
        org-html-themify--backup-org-html-head-extra org-html-head-extra
        org-html-themify--backup-org-html-postamble org-html-postamble
        org-html-themify--backup-org-html-preamble org-html-preamble)
  (setq org-html-head-include-default-style nil)
  (setq htmlize-face-overrides
        (-concat
         org-html-themify--backup-htmlize-face-overrides
         '(font-lock-keyword-face
	   (:foreground "var(--clr-keyword)" :background "var(--bg-keyword)")
           font-lock-constant-face
	   (:foreground "var(--clr-constant)" :background "var(--bg-constant)")
           font-lock-comment-face
	   (:foreground "var(--clr-comment)" :background "var(--bg-comment)")
           font-lock-comment-delimiter-face
	   (:foreground "var(--clr-comment-delimiter)" :background "var(--bg-comment-delimiter)")
           font-lock-function-name-face
	   (:foreground "var(--function-clr-name)" :background "var(--function-bg-name)")
           font-lock-variable-name-face
	   (:foreground "var(--clr-variable)" :background "var(--bg-variable)")
           font-lock-preprocessor-face
	   (:foreground "var(--clr-preprocessor)" :background "var(--bg-preprocessor)")
           font-lock-doc-face
	   (:foreground "var(--clr-doc)" :background "var(--bg-doc)")
           font-lock-builtin-face
	   (:foreground "var(--clr-builtin)" :background "var(--bg-builtin)")
           font-lock-string-face
	   (:foreground "var(--clr-string)" :background "var(--bg-string)")))))

(defun org-html-themify--uninit ()
  (remove-hook 'org-export-before-processing-hook 'org-html-themify--setup-inlines)
  (setq org-html-head-include-default-style t)
  (setq htmlize-face-overrides org-html-themify--backup-htmlize-face-overrides
        org-html-head-extra org-html-themify--backup-org-html-head-extra
        org-html-postamble org-html-themify--backup-org-html-postamble
        org-html-preamble org-html-themify--backup-org-html-preamble))

(define-minor-mode org-html-themify-mode
  "Themify org-mode HTML export with Emacs color theme."
  :version 2.0
  (if org-html-themify-mode
      (org-html-themify--init)
    (org-html-themify--uninit)))

;;; original org-html-themify ends here.

;; This enables a copy-to-clipboard button org source blocks
;; https://emacs.stackexchange.com/questions/31260/what-would-be-the-simplest-way-to-add-a-copy-to-clipboard-button-to-html-expor
(defun et-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML, adding a 'copy to clipboard' button."
  (if (not (org-export-read-attribute :attr_html src-block :copy-button))
      (org-export-with-backend 'html src-block contents info)
    (let*((b-id (concat "btn_" (s-replace "-" "" (uuidgen-4))))
          (content (let ((print-escape-newlines t))
		     (prin1-to-string (org-export-format-code-default src-block info))))
          (content- (s-chop-prefix "\"" (s-chop-suffix "\"" (s-replace "`" "\\`" content))))
          (btn- "button")
          (scr- "script")
          (bquote- "`")
          (script
	   (concat "\n<" scr- " type='text/javascript'>\n var copyBtn"
		   b-id "=document.querySelector('" btn- "[name=" b-id "]');\n"
                   "copyBtn" b-id ".addEventListener('click', function(event) {\n"
                   "copyTextToClipboard(" bquote- content- bquote- ");\n});\n</" scr- ">\n"))
          (button
	   (concat "<" btn- " class='copyBtn' name=" b-id ">copy</" btn- ">")))
      (concat "<div class='org-src-wrapper'>\n "
	      (org-export-with-backend 'html src-block contents info)
	      button script
	      "</div>"))))

(org-export-define-derived-backend 'et-html 'html
  :translate-alist '((src-block . et-html-src-block)))

;; extends ox-html function org-html-publish-to-html to new backend
(defun et-org-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML, adding 'copy' buttons to source blocks.

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


(provide 'org-html-themify)

