;; -*- lexical-binding: t -*-
;; User Interface configuration
;==============================================================================
(defun et-init-gui (config)
  "Set up Ewan's UI"
  (et-init-frame)
  (et-init-fileio)
  (et-init-globals)
  (et-init-ergonomics)
  (et-init-facing config))

(defun et-init-frame ()
  (setq inhibit-splash-screen t
	frame-resize-pixelwise t
	default-frame-alist
	'((top . 0) (left . 0)
	  (width . 83) (height . 50)
	  (tool-bar-lines . 0)
	  (internal-border-width . 0)
	  (vertical-scroll-bars . nil)
	  (horizontal-scroll-bars . nil)))
  (global-hl-line-mode 1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode nil))

(defun et-init-facing (config)
  "Initialize prettier Emacs"
  (interactive)
  (let ((themes (concat config "/themes")))
    (add-to-list 'load-path themes)
    (add-to-list 'custom-theme-load-path themes)
    (load-theme 'tomorrow-night-eighties t)
    (set-face-foreground 'fill-column-indicator "grey30")
    (with-system darwin
      (add-to-list 'default-frame-alist '(font . "Menlo 13")))))

(defun et-init-fileio ()
  "Initialize buffer interaction with filesystem"
  (interactive)
  (put 'erase-buffer 'disabled nil)
  (auto-save-visited-mode)
  (global-auto-revert-mode)
  (let* ((versions (concat user-emacs-directory "versions/"))
	 (undos (concat versions "undos/"))
	 (backups (concat versions "backups/"))
	 (autosaves (concat versions "autosaves/")))
    (progn
      (dolist (dir (list versions undos backups autosaves))
	(unless (file-directory-p dir) (make-directory dir)))
      (setq backup-directory-alist `(("." . ,backups)))
      (setq auto-save-file-name-transforms
	    `(("\\(.+/\\)*\\(.*?\\)" ,(concat autosaves "\\2"))))
      (setq auto-save-list-file-prefix (concat autosaves ".saves-"))
      (use-package undo-tree
	:bind
	(("C-/" . undo-tree-undo)
	 ("C-_" . undo-tree-undo)
	 ("C-M-/" . undo-tree-redo)
	 ("C-M-_" . undo-tree-redo))
	:config
	(progn
	  (setq undo-tree-history-directory-alist `((".*" . ,undos)))
	  (global-undo-tree-mode))))))

(defun et-init-ergonomics ()
  "Initialize general gui-interaction tools"
  (interactive)
  (use-package tab-bar
    :init (define-prefix-command 'et/tabs)
    :bind (:map et/tabs
		("f" . tab-next)
		("b" . tab-previous)
		("n" . tab-new)
		("s" . tab-switch)
		("k" . tab-close)
		("r" . tab-rename))
    :config
    (setq tab-bar-mode t
	  tab-bar-new-button-show nil
	  tab-bar-close-button-show nil)
    (set-face-attribute 'tab-bar nil ;; background bar
			:background "#2B2B2B"
			:foreground "#DCDCCC"
			:distant-foreground "#DCDCCC"
			:box "#494949"
			:height 0.5
			:family "Menlo")
    (set-face-attribute 'tab-bar-tab nil ;; active tab
			:background "#9FC59F"
			:foreground "#9FC59F"
			:distant-foreground "#9FC59F"
			:box "#494949")
    (set-face-attribute 'tab-bar-tab-inactive nil ;; inactive tab
			:background "#5F7F5F"
			:foreground "#5F7F5F"
			:distant-foreground "#5F7F5F"
			:box "#494949"))
  (use-package windmove
    :bind (("C-c <left>" . windmove-left)
	   ("C-c <right>" . windmove-right)
	   ("C-c <up>" . windmove-up)
	   ("C-c <down>" . windmove-down)))
  (use-package buffer-move
    :bind (("C-c M-<up>" . buf-move-up)
	   ("C-c M-<down>" . buf-move-down)
	   ("C-c M-<left>" . buf-move-left)
	   ("C-c M-<right>" . buf-move-right)))
  (use-package winner
    :bind (("C-s-/" . winner-undo)
	   ("C-s-?" . winner-redo)))
  (use-package which-key
    :config (which-key-mode))
  (use-package helm
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ;("C-x M-f" . helm-find)
	 ("C-x M-f" . (lambda () (interactive) (helm-find "P")))
	 ("C-x M-b" . list-buffers)
	 ("C-x C-b" . helm-buffers-list))))

(defun et-init-globals ()
  "Initialize global keybindings."
  (interactive)
  (global-unset-key (kbd "C-;"))
  (global-set-key   (kbd "C-;") 'set-mark-command)
  (global-set-key (kbd "M-n") 'forward-paragraph)
  (global-set-key (kbd "M-p") 'backward-paragraph)
  (global-unset-key (kbd "C-,"))
  (global-unset-key (kbd "C-."))
  (global-set-key (kbd "C-.") 'end-of-buffer)
  (global-set-key (kbd "C-,") 'beginning-of-buffer)
  ; FN2-... : manage buffers
  (global-set-key (kbd "M-s-[") 'previous-buffer)
  (global-set-key (kbd "M-s-]") 'next-buffer)
  (global-set-key (kbd "M-s-'") 'switch-to-buffer)
  (global-unset-key (kbd "M-s-<right>"))
  (global-unset-key (kbd "M-s-<left>"))
  (global-unset-key (kbd "M-s-<down>"))
  (global-unset-key (kbd "M-s-<up>"))
  (global-set-key (kbd "M-s-<up>") 'enlarge-window)
  (global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "M-s-<down>") 'shrink-window)
  (global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
  ; FN3-<arrow> : manage point
  ; Arrows windmove between buffers
  ; Chords - not needed yet
  ; Max delay between two key presses to be chord
  ;(setq key-chord-two-keys-delay 0.1) ; default 0.1
  ; Max delay between two presses of the same key to be chord.
  ;(setq key-chord-one-key-delay 0.2)  ; default 0.2
  ;(key-chord-mode 1)
  )

;==============================================================================
(provide 'gui-config)
