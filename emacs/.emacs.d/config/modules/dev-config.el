;; -*- lexical-binding: t -*-
;; Development configuration
;==============================================================================
(require 'utils)

(defun init-dev (stem langs)
  "Initialize development environments"
  (init-prog)
  (mapc (lambda (sym) (funcall (symbol-function sym))) langs))

;==============================================================================
;; General programming config

(defun schemify (a-lisp2-mode-map)
  (progn (define-key a-lisp2-mode-map (kbd "C-M-y")		     
		     (lambda () (interactive)
		       (progn (insert "(lambda ())") (backward-char 2))))
	 (define-key a-lisp2-mode-map (kbd "C-M-;")
		     (lambda () (interactive) (progn (insert "funcall "))))))

(defun init-prog ()
  "Initialize all programming modes"
  (message "Initializing prog modes")
  (use-package magit)
  ;(use-package projectile)
  (use-package helm-projectile
    :bind ("C-x C-p" . 'helm-projectile))
  ;; (use-package treemacs
  ;;   :bind ("C-c C-t" . 'treemacs)
  ;;   :config
  ;;   (progn
  ;;     (setq treemacs-no-png-images t)
  ;;     (setq treemacs-filewatch-mode t)))
  (use-package smartparens
    :config
    (require 'smartparens-config))
  (use-package company
    :config
    (setq company-frontends
	  '(company-pseudo-tooltip-unless-just-one-frontend
	    company-preview-frontend
	    company-echo-metadata-frontend))
    (define-key company-active-map (kbd "<tab>") 'company-complete-common))
  (use-package flycheck
    :config
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (use-package tree-sitter)
  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all))
  (use-package eglot
    :custom
    (eglot-autoshutdown t)
    (eglot-events-buffer-size 0)
    (eglot-extend-to-xref nil)
    (eglot-ignored-server-capabilities
     '(;hoverProvider
       documentHighlightProvider
       documentFormattingProvider
       documentRangeFormattingProvider
       documentOnTypeFormattingProvider
       colorProvider
       ;foldingRangeProvider
       )))
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (display-line-numbers-mode 1)
	      (setq fill-column 80)
	      (display-fill-column-indicator-mode 1)
	      (smartparens-mode 1)
	      (company-mode 1)
	      ;(flycheck-mode 1)
	      (treesit-auto-mode 1)
	      (setq prettify-symbols-alist
		    '(("lambda" . 955) ;?λ
		      ("funcall " . ?:)
		      ))
	      (prettify-symbols-mode 1)
	      (schemify emacs-lisp-mode-map)	    
	      )))

;===============================================================================
;; Lisps/Schemes/etc.

(defun l-racket ()
  "Initialize Racket dev env"
  (message "Initializing Racket mode")
  (use-package racket-mode
    :config
    (racket-unicode-input-method-enable)
    (setq racket-images-inline t)
    (add-hook 'racket-mode-hook 'racket-xp-mode)
    (let ((def-racket-key
	   (lambda (str fun &optional repl)
	     (progn
	       (define-key racket-mode-map (kbd str) fun)
	       (when repl (define-key racket-repl-mode-map (kbd str) fun))))))
      (progn (funcall def-racket-key "C-M-y"
		(lambda () (interactive)
		  (progn (insert "(lambda ())") (backward-char 2))))
	     (funcall def-racket-key "C-x C-e" 'racket-eval-last-sexp)
	     (funcall def-racket-key "C-M-<return>" 'racket-run)
	     (funcall def-racket-key "C-c t" (lambda () (interactive) (insert "⊤")) t)
	     (funcall def-racket-key "C-c f" (lambda () (interactive) (insert "⊥")) t)
	     (funcall def-racket-key "C-c n" (lambda () (interactive) (insert "¬")) t)
	     (funcall def-racket-key "C-c a" (lambda () (interactive) (insert "∧")) t)
	     (funcall def-racket-key "C-c o" (lambda () (interactive) (insert "∨")) t)
	     (funcall def-racket-key "C-c p" (lambda () (interactive) (insert "φ")) t)
	     (funcall def-racket-key "C-c e" (lambda () (interactive) (insert "≡")) t)))))

(defun l-chez ()
  "Initialize Chez Scheme dev env"
  (message "Initializing Chez Scheme mode")
  (use-package geiser-chez
    :hook scheme-mode
    :bind
    (:map scheme-mode-map
	  ("C-x C-e" . geiser-eval-last-sexp)
	  ("C-M-y" .
	   (lambda () (interactive)
	     (progn (insert "(lambda ())") (backward-char 2)))))))

(defun l-sbcl ()
  "Initialize SBCL (Common Lisp) dev env (Sly)"
  (message "Initializing Common Lisp mode")
  (use-package sly
    :bind
    (:map sly-mode-map
	  ("C-c C-f" . 
	   (lambda ()
	     "Compile and print to mrepl"
	     (interactive)
	     (let* ((form (sly-sexp-at-point))
		    (form-with-print (format "(print %s)" form))
		    (sly-command (sly-interactive-eval form-with-print)))
	       (sly-compile-defun)
	       (message "Compiled: %s" form-with-print)))))
    :config
    ;(setq inferior-lisp-program "sbcl")
    (setq sly-lisp-implementations '((sbcl ("sbcl") :coding-system utf-8))
	  sly-default-lisp 'sbcl
	  sly-command-switch-to-existing-lisp 'always
	  sly-auto-select-connection 'always)
    (add-hook 'emacs-lisp-mode-hook (lambda () (sly-mode -1)))
    (add-hook 'mrepl-mode-hook 'company-mode)
    (sly-symbol-completion-mode -1)
    (add-hook 'sly-mode-hook 'company-mode)
    (schemify lisp-mode-map)
    (schemify sly-mode-map))
  (use-package sly-quicklisp))

(use-package paredit
  :hook (lisp-mode emacs-lisp-mode scheme-mode sly-mode))

;===============================================================================
;; JS/TS

(defun l-typescript ()
  "Initialize TypeScript dev env"
  (message "Initializing TypeScript mode")
  (ignore-errors (eglot-ensure))
  (assq-delete-all 'typescript-mode eglot-server-programs)
  (add-to-list 'eglot-server-programs
	       '((typescript-mode) "typescript-language-server" "--stdio"))
  ;; Typescript project find fix copied from https://notes.alexkehayias.com
  (cl-defmethod project-root ((project (head eglot-project))) (cdr project))
  (add-hook 'project-find-functions
	    (lambda (dir)
	      (when-let* ((dir (locate-dominating-file dir "tsconfig.json")))
		(cons 'eglot-project dir))))
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (progn (setq indent-tabs-mode nil)
		     (setq tab-width 4)
		     (setq typescript-ts-mode-indent-offset 4)))))

(defun l-javascript ()
  "Initialize JavaScript dev env"
  (message "Initializing JavaScript mode")
  ;; Use TS modes for JS editing
  (assq-delete-all 'javascript-mode eglot-server-programs)
  (add-to-list 'auto-mode-alist '("\\.js$" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . tsx-ts-mode))
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (l-typescript))

(defun l-web ()
  (use-package web-mode
    :mode ("\\.html\\'" "\\.php\\'" "\\.erb\\'")
    :config
    (setq web-mode-markup-indent-offset 2
	  web-mode-css-indent-offset 2
	  web-mode-code-indent-offset 2
	  web-mode-enable-auto-pairing t
	  web-mode-enable-css-colorization t
	  web-mode-enable-current-element-highlight t)))

;==============================================================================

(provide 'dev-config)
