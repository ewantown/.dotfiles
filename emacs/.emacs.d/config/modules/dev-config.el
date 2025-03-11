
;; -*- lexical-binding: t -*-
;; Development configuration
;;==============================================================================
(require 'utils)

(defun et-init-dev (stem langs)
  "Initialize development environments"
  (et-init-prog)
  (mapc (lambda (sym) (funcall (symbol-function sym))) langs))

;;==============================================================================
;; General programming config

(defun et-schemify-mode-map (a-lisp2-mode-map)
  (progn (define-key a-lisp2-mode-map (kbd "C-M-y")		     
		     (lambda () (interactive)
		       (progn (insert "(lambda ())") (backward-char 2))))
	 (define-key a-lisp2-mode-map (kbd "C-M-;")
		     (lambda () (interactive)
		       (progn (insert "(funcall )") (backward-char 1))))))

(defun et-init-prog ()
  "Initialize all programming modes"
  (interactive)
  (message "Initializing prog modes")
  (use-package magit)
					;(use-package projectile)
  (use-package helm-projectile
    :bind ("C-x C-p" . 'helm-projectile))
  (use-package treemacs
    :bind ("C-c C-t" . 'treemacs)
    :config
    (setq treemacs-no-png-images t)
    (setq treemacs-filewatch-mode t))
  (use-package smartparens
    :config
    (require 'smartparens-config))
  (use-package paredit
    :hook (emacs-lisp-mode lisp-mode scheme-mode)
    :bind
    (:map paredit-mode-map
	  ("M-<right>" . paredit-forward-slurp-sexp)
	  ("M-<left>" . paredit-forward-barf-sexp)))
  (use-package company
    :bind
    (:map company-active-map
	  ("TAB" . company-complete-common))
    :config
    (setq company-frontends
	  '(company-pseudo-tooltip-unless-just-one-frontend
	    company-preview-frontend
	    company-echo-metadata-frontend)))
  (use-package flycheck
    :config
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (use-package tree-sitter)
  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all))
  (use-package restclient
    :mode ("\\.http\\'" . restclient-mode))
  (use-package eglot
    :custom
    (eglot-autoshutdown t)
    (eglot-events-buffer-size 0)
    (eglot-extend-to-xref nil)
    (eglot-ignored-server-capabilities
     '(;;hoverProvider
       documentHighlightProvider
       documentFormattingProvider
       documentRangeFormattingProvider
       documentOnTypeFormattingProvider
       colorProvider
       ;;foldingRangeProvider
       )))
  (add-hook 'prog-mode-hook
	    (lambda ()
	      (display-line-numbers-mode 1)
	      (setq fill-column 80)
	      (display-fill-column-indicator-mode 1)
	      (smartparens-mode 1)
	      (company-mode 1)
	      (flymake-mode -1)
	      (flycheck-mode -1)
	      (treesit-auto-mode 1)
	      (setq prettify-symbols-alist
		    '(("lambda" . 955)	;?λ
		      ("funcall" . ?⦿)))		      
	      (prettify-symbols-mode 1)
	      (et-schemify-mode-map emacs-lisp-mode-map))))

;;============================================================================
;; Lisps/Schemes/etc.

(defun l-racket ()
  "Initialize Racket dev env"
  (interactive)
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
		 (progn (insert "(λ ())") (backward-char 2))))
	     (funcall def-racket-key "C-x C-e" 'racket-eval-last-sexp)	     
	     (funcall def-racket-key "C-M-<return>" 'racket-run)	     
	     (funcall def-racket-key "C-c r" (lambda () (interactive) (insert "ρ")) t)
	     (funcall def-racket-key "C-c s" (lambda () (interactive) (insert "σ")) t)
	     (funcall def-racket-key "C-c d" (lambda () (interactive) (insert "•")) t)
	     (funcall def-racket-key "C-c -" (lambda () (interactive) (insert "→")) t)
	     (funcall def-racket-key "C-c c" (lambda () (interactive) (insert "▷")) t)	     
	     (funcall def-racket-key "C-c e" (lambda () (interactive) (insert "ε")) t) 
	     (funcall def-racket-key "C-c b" (lambda () (interactive) (insert "□")) t)
	     (funcall def-racket-key "C-c t" (lambda () (interactive) (insert "⊤")) t)
	     (funcall def-racket-key "C-c f" (lambda () (interactive) (insert "⊥")) t)
	     (funcall def-racket-key "C-c n" (lambda () (interactive) (insert "¬")) t)
	     (funcall def-racket-key "C-c a" (lambda () (interactive) (insert "∧")) t)
	     (funcall def-racket-key "C-c o" (lambda () (interactive) (insert "∨")) t)
	     (funcall def-racket-key "C-c p" (lambda () (interactive) (insert "φ")) t)
	     (funcall def-racket-key "C-c =" (lambda () (interactive) (insert "≡")) t)))))

(defun l-chez ()
  "Initialize Chez Scheme dev env"
  (interactive)
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
  (interactive)
  (message "Initializing Common Lisp mode")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  (et-schemify-mode-map lisp-mode-map))

(defun l-clojure ()
  "Initialize Clojure dev env"
  (interactive)
  (message "Initializing Clojure mode")
  (use-package clojure-mode)
  (use-package cider))
;;===============================================================================
;; JS/TS

(defun l-latex ()
  "Initialize latex environment"
  (interactive)
  (message "Initializing LaTeX mode")	       
  (add-to-list 'eglot-server-programs
	       '((latex-mode) "texlab")))

(defun l-typescript ()
  "Initialize TypeScript dev env"
  (interactive)
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
  (interactive)
  (message "Initializing JavaScript mode")
  ;; Use TS modes for JS editing
  (assq-delete-all 'javascript-mode eglot-server-programs)
  (add-to-list 'auto-mode-alist '("\\.js$" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . tsx-ts-mode))
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (l-typescript))

(defun l-web ()
  "Initialize web-dev env"
  (interactive)
  (use-package web-mode
    :mode ("\\.html\\'" "\\.php\\'" "\\.erb\\'")
    :config
    (setq web-mode-markup-indent-offset 2
	  web-mode-css-indent-offset 2
	  web-mode-code-indent-offset 2
	  web-mode-enable-auto-pairing t
	  web-mode-enable-css-colorization t
	  web-mode-enable-current-element-highlight t)))

;;==============================================================================
;; SQL
(defun l-sql ()
  "Initialize SQL env"
  (interactive)
  (use-package sql
    :config
    (setq sql-db2-program "/mnt/d/SQLLIB/BIN/db2cmd.exe"
	  sql-db2-options '("-c" "-i" "-w" "db2" "-tv"))
    (defalias 'sql-get-login 'ignore)	; login with connection string
    (advice-add 'sql-send-paragraph :before 'et-db-connect)))

(defconst et-dbms-alist
  '(("DB2" . sql-db2)))

(defun et-sql-connection-string (dbms inst)
  "Construct SQL connection string for INST using environment vars for DBMS"
  (let ((user (getenv (concat (upcase dbms) "_USER")))
	(pass (getenv (concat (upcase dbms) "_PASS"))))
    (match (upcase dbms)
	   ("DB2"
	    (concat "connect to '" inst "' user '" user "' using '" pass "';"))
	   ;; add more as needed
	   (_ (error (format "No matching DBMS for %s" dbms))))))

(defun et-db-connect ()
  "Get DB connection details from user and connect if not already"
  (interactive)
  (unless (sql-buffer-live-p sql-buffer)
    (let* ((dfdbms "DB2")
	   (dbms
	    (read-string
	     (format "DBMS (default %s): " dfdbms) nil nil dfdbms nil))
	   (inst (read-string "DB Instance: "))
	   (buff (concat "*SQL: " (upcase dbms) " - " (upcase inst) "*")))
      (let ((new (not (get-buffer buff)))
	    (window (selected-window)))
	(progn
	  (funcall (cdr (assoc dbms et-dbms-alist)) buff)
	  (if new
	      (progn
		(switch-to-buffer buff)
		(toggle-truncate-lines 1)
		(comint-clear-buffer)
		(insert (et-sql-connection-string dbms inst))
		(comint-send-input)))
	  (select-window window))))))
;;==============================================================================
;; C# / .NET


;; (defun l-csharp ()
;;   (use-package csharp-mode
;;     :mode ("\\.cs\\'")
;;     :init
;;     (let ((path "~/.dotnet/tools/csharp-ls"))
;;       ;;(add-to-list 'eglot-server-programs '(csharp-mode . ("csharp-ls")))
;;       ;;(add-to-list 'eglot-server-programs '(csharp-ts-mode . ("csharp-ls")))
;;       (setcdr (assoc '(csharp-mode csharp-ts-mode) eglot-server-programs)
;; 	      `(,path))
;;       (setcdr (assoc 'csharp-mode eglot-server-programs) `(,path))
;;       (setcdr (assoc 'csharp-ts-mode eglot-server-programs) `(,path)))
;;     :hook ((csharp-mode csharp-ts-mode) . eglot-ensure)))

;;==============================================================================
;; Markup langs
(defun l-xml ()
  (use-package nxml
	:mode ("\\.xml\\'" "\\.uim\\'" "\\.vim\\'")
	:hook  (nxml-mode . et-xml-format)))

(defun et-xml-format ()
  (interactive)
  (setq
   tab-width 4
   nxml-child-indent 4
   nxml-attribute-indent 4
   nxml-slash-auto-complete-flag t))

(defun l-yaml ()
  (use-package yaml-mode
	:mode ("\\.yml\\'" "\\.yaml\\'")))

;;==============================================================================
;; Java
(defun l-java ()  
  (setcdr (assoc '(java-mode java-ts-mode) eglot-server-programs)
          `(
	    "/mnt/c/Program Files/Git/bin/bash.exe -c java"
            "-Declipse.application=org.eclipse.jdt.ls.core.id1"
            "-Dosgi.bundles.defaultStartLevel=4"
            "-Declipse.product=org.eclipse.jdt.ls.core.product"
            "-Dlog.level=ALL"
            "-noverify"
            "-Xmx256m"
            "-jar"
            "/c/Eclipse462/dropins/jdtls-jdk8/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar"
            "-configuration"
            "/c/Eclipse462/dropins/jdtls-jdk8/config_win"
            "-data"
            "/c/Users/ET20469/jdtls-cache"
	    ;;"jdtls" "-data" "/home/user/.cache/emacs/workspace/"
            ;;"-javaagent:/home/user/work/src/lombok.jar"
            ;;"-Xbootclasspath/a:/home/user/work/src/lombok.jar"
            ;;"--jvm-arg=-XX:+UseG1GC"
            ;;"--jvm-arg=-XX:+UseStringDeduplication"
            ;;"-Djava.format.settings.url=file:///home/user/code-format.xml"
            ;;"-Djava.format.settings.profile=myown"
	    )))

;;==============================================================================
(provide 'dev-config)
