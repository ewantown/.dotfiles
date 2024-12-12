;; -*- lexical-binding: t -*-
;; Miscellaneous configuration
;==============================================================================
(defun et-init-etc (stem)
  "Initialize miscellaneous tools"  
  (use-package eww
    :bind (("C-c w" . eww)
           :map eww-mode-map
           ("C-q" . quit-window)
           ("C-l" . 'eww-lnum-follow))
    :config
    (setq browse-url-browser-function
	  (if (display-graphic-p)
	      'browse-url-default-browser
	    'eww-browse-url)))
  (use-package bookmark
    :bind (("M-s-s" . bookmark-set)
           ("M-s-g" . bookmark-jump)
           ("M-s-b" . bookmark-bmenu-list)))
  (use-package dabbrev
    :bind (("M-m" . electric-newline-and-maybe-indent)
           ("C-<tab>" . dabbrev-expand))
    :config
    (setq dabbrev-case-distinction nil
          dabbrev-case-fold-search nil
          dabbrev-case-replace nil
          dabbrev-upcase-means-case-search t))
  (et-init-ai-tools))

;; (use-package pdf-tools
;;   :config
;;   (progn
;;     (pdf-tools-install)
;;     (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
;;     (add-hook 'pdf-view-mode-hook (lambda()
;;                                     (pdf-view-midnight-minor-mode 1)))))

;===============================================================================
(defun et-init-ai-tools ()
    (use-package gptel
      :config
      (progn
	(setq gptel-model "mistralai/Mixtral-8x7B-Instruct-v0.1"
              gptel-backend
              (gptel-make-openai "HAL"
		:host "api.together.xyz"
		:key (getenv "TAI_KEY")
		:stream t
		:models '("mistralai/Mixtral-8x7B-Instruct-v0.1"
			  "codellama/CodeLlama-13b-Instruct-hf"
			  "codellama/CodeLlama-34b-Instruct-hf"))))))


;(use-package wolfram
;  :config
;  (setq wolfram-alpha-app-id "43X66J-3RL7UAKAQ3"))


;; (define-prefix-command 'et/chatgpt-map)
;; (use-package chatgpt-shell
;;   :config

;;   (add-to-list 'exec-path "~/.emacs.d/config/packages/chatgpt-shell")
;;   (setq chatgpt-shell-openai-key (getenv "OPENAI_KEY))
;;   (setq chatgpt-shell-welcome-function nil)
;;   (setq chatgpt-shell-model-version "gpt-4-0125-preview");"gpt-3.5-turbo")
;;   :bind (:map et/chatgpt-map
;;               ("s" . chatgpt-shell-in-new-buffer)
;;               ("c" . chatgpt-generate-code-snippet)
;; 	          ("m" . chatgpt-calc)
;;               ("e" . chatgpt-shell-explain-code)
;;               ("p" . chatgpt-shell-prompt)
;;               ("DEL" . chatgpt-shell-clear-buffer)))
 
;; (defun chatgpt-shell-in-new-buffer ()
;;   "Create a new window and start a new shell in it."
;;   (interactive)
;;   (let ((new-window (split-window)))
;;     (select-window new-window)
;;     (call-interactively 'chatgpt-shell)
;;     (goto-char (point-max))))

;; (defun chatgpt-generate-code-snippet ()
;;   "Generate code to specified language and functionality."
;;   (interactive)
;;   (let* ((lang (read-string "Language: "))
;;          (func (read-string "Functionality: "))
;;          (prel (concat "You are just a code generator. "
;; 		       "Respond to this only with " lang " code. "
;; 		       "Verify that your code is syntactically correct. "
;;                        "Pretty-print the code. "
;;                        "Include no preamble, nor description, nor markdown. "))
;;          (prmt (concat prel  "Write code in " lang " to " func)))
;;     (chatgpt-shell-send-to-buffer prmt)))

;; (defun chatgpt-calc ()
;;   "Perform calculation and print result to buffer"
;;   (interactive)
;;   (let* ((mathq (read-string "Calculate: "))
;;          (prel (concat "You are just a smart calculator. "
;; 		       "Respond to this only with the results of calculation. "
;;                        "Format all answers in standard mathematical notation. "
;;                        "Include no preamble, nor description, nor markdown."))
;;          (prmt (concat prel  "Calculate: " mathq)))
;;     (chatgpt-shell-send-to-buffer prmt)))


;==============================================================================
;; Remote
;; (use-package tramp
;;   :config
;;   (progn
;;     (setq tramp-default-method "ssh")
;;     (setq tramp-default-user "eat")
;;     (setq tramp-default-host "remote.students.cs.ubc.ca")
;;     (setq tramp-default-directory "~/")
;;     (setq tramp-ssh-controlmaster-options
;; 	  "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")))

;; (setq sql-default-directory "/ssh:remote.students.cs.ubc.ca:~")
;; (setq sql-oracle-program "/cs/local/generic/bin/sqlplus");
;; (setq sql-oracle-login-params '("ora_eat" "a37371242" "stu"))
;(add-hook 'sql-login-hook 'login-hook)
;(defun login-hook ()
;  "Custom SQL log-in behaviours. See `sql-login-hook'."
;  (setq sql-prompt-regexp ".*")
;  (let ((proc (get-buffer-process (current-buffer))))
;    (comint-send-string proc "ora_eat/a37371242@stu\n")))

;==============================================================================
(setq erc-server "irc.libera.chat"
      erc-nick "etown"    
      erc-user-full-name "ET"      
      erc-track-shorten-start 8
      erc-autojoin-mode 1
      erc-autojoin-channels-alist
      '(("irc.libera.chat" "#systemcrafters" "#emacs" "#lisp"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)
;==============================================================================
(provide 'etc-config)
