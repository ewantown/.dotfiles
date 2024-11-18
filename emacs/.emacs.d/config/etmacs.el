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
      (list CONF MODULES DROPINS))

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

(with-system darwin
  (setq mac-option-modifier 'meta)
  (add-to-list 'exec-path "/usr/local/bin"))

(with-system windows-nt
  (setq w32-apps-modifier 'super)
  ;(add-to-list 'exec-path "C:/Program\ Files")
  ; So C-x C-c exits terminal emacs under git bash:
  (global-set-key [24 pause] (quote save-buffers-kill-terminal)))

(with-system gnu/linux
  ; TODO - port general linux config
  (when (getenv "WSL_DISTRO_NAME")
    (progn (require 'wsl-config) (init-wsl CONF STEM))))

;==============================================================================
(require 'gui-config) ; user interface
(require 'dev-config) ; development
(require 'org-config) ; org-mode
(require 'etc-config) ; other
(require 'wsl-config) ; wsl
(require 'patches)
(patch)

;; Note: langs name functions defined and referenced in dev-config.el
(defconst LANGS '(l-chez l-sbcl l-javascript l-typescript))

;; Main dispatch
(if (display-graphic-p)
    (progn (init-gui CONF)
	   (init-org STEM)
	   (init-dev STEM LANGS)
	   ;(init-etc STEM)
	   )
    (progn (fileio CONF)
	   (globals)
	   (ergonomics)
	   (add-hook 'after-init-hook 'shell)
	   ))

;==============================================================================

(setq byte-compile-warnings '()) ; errors only
(when (equal 1 (random 10))
  (byte-recompile-directory (concat user-emacs-directory "elpa/") 0))

(defun clock-startup ()
  (message "*** Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time (time-subtract after-init-time
					      before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook 'clock-startup)

;==============================================================================
