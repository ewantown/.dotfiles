;; -*- lexical-binding: t -*-
;; Entry point to etmacs
;==============================================================================
(setq debug-on-error t)
(prefer-coding-system 'utf-8)

(setq user-full-name "Ewan Townshend"
      user-mail-address "ewan@etown.dev")

;==============================================================================
(defconst STEM "~/-/")
(defconst CONF (concat user-emacs-directory "config/"))
(defconst MODULES (concat CONF "modules/"))
(defconst DROPINS (concat CONF "dropins/"))

(mapc (lambda (dir) (add-to-list 'load-path dir)) 
      (delete-dups (mapcar (lambda (x) (file-name-directory x))
			   (directory-files-recursively CONF "**"))))

(setq custom-file (concat CONF "custom.el"))

(let ((env (concat CONF ".env")))
  (when (file-exists-p env)
    (load-file env)))

;==============================================================================
(require 'package)
(setq package-enable-at-startup t)
(setq package-archives
      '(
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	;("jcs-elpa" . "http://jcs-emacs.github.io/jcs-elpa/packages/")
	))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;==============================================================================
(require 'utils)

(defvar LANGS '() "Function symbols to be called by development configuration")

(with-system darwin
  (setq mac-option-modifier 'meta)
  (add-to-list 'exec-path "/usr/local/bin")
  (setq LANGS '(l-chez l-sbcl l-javascript l-typescript)))

(with-system windows-nt
  (setq w32-apps-modifier 'super)
  ;(add-to-list 'exec-path "C:/Program\ Files")
  ; So C-x C-c exits terminal emacs under git bash:
  (global-set-key [24 pause] (quote save-buffers-kill-terminal))
  (setq LANGS '()))

(with-system gnu/linux
  ; TODO - port general linux config
  (when (getenv "WSL_DISTRO_NAME")
    (progn (require 'wsl-config) (et-init-wsl CONF STEM))
    (setq LANGS '(l-csharp l-yaml l-xml l-sql))))

;==============================================================================
(require 'gui-config) ; user interface
(require 'dev-config) ; development
(require 'org-config) ; org-mode
(require 'etc-config) ; other
(require 'patches)
(patch)

;; Main dispatch
(if (display-graphic-p)
    (progn (et-init-gui CONF)
	   (et-init-org STEM)
	   (et-init-etc STEM)
	   (et-init-dev STEM LANGS)
	   )
  (progn (load-theme 'modus-vivendi)
	 (et-init-frame)
	 (et-init-fileio)
	 (et-init-globals)
	 (et-init-ergonomics)
	 (add-hook 'after-init-hook 'shell)
	 ))
;==============================================================================

(setq byte-compile-warnings '()) ; errors only
(when (equal 1 (random 10))
  (byte-recompile-directory (concat user-emacs-directory "elpa/") 0))

(defun et-clock-startup ()
  (message "*** Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time (time-subtract after-init-time
					      before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook 'et-clock-startup)

;==============================================================================
