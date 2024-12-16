;; -*- lexical-binding: t -*-
;; WSL Configuration
;==============================================================================
;; stub
(defun et-init-wsl (conf stem)
  "Initialize on Windows Subsystem for Linux"
  (et-init-wsl-env)
  (when (not (display-graphic-p))
    (et-init-wsl-nw-clipboard)
    (et-init-git-switcher))
  (use-package sudo-edit
    :bind ("C-c C-r" . 'sudo-edit)))
;==============================================================================
(defun et-init-wsl-env ()
  (setenv "LOCALHOST"
	  (let ((sout (shell-command-to-string "ip route")))
      	    (progn (string-match "default via \\([0-9.]+\\)" sout)
		   (match-string 1 sout)))))

(defun et-init-wsl-nw-clipboard ()
    (use-package xclip
      :config (xclip-mode 1)))

(defun et-set-magit-executable-for-dir (&optional x &rest _)
  "Set the appropriate Git executable for the current directory."
  (if (string-prefix-p "/mnt/" default-directory)
      (progn
	(setq magit-git-executable "/mnt/c/Program Files/Git/bin/git.exe")
	(shell-command "git config --global core.editor \"emacsclient -c -a ''\""))
    (setq magit-git-executable "/usr/bin/git")))

(defun et-set-git-config-for-magit ()
  "Set the Git config 'core.editor' to use the Emacs internal editor."
  (shell-command "git config --global core.editor \"emacsclient -c -a ''\""))

(defun et-init-git-switcher ()  
  (advice-add 'magit-status :before #'et-set-magit-executable-for-dir)
  (advice-add 'magit-commit :before #'et-set-magit-executable-for-dir)
  (advice-add 'magit-push :before #'et-set-magit-executable-for-dir)
  (advice-add 'magit-pull :before #'et-set-magit-executable-for-dir)
  (advice-add 'magit-fetch :before #'et-set-magit-executable-for-dir))

(defun git-bash ()
  (interactive)
  (ansi-term "\"/mnt/c/Program Files/Git/bin/bash.exe\"" "*Git-bash*"))
(defun cmd ()
  (interactive)
  (ansi-term "\"/mnt/c/Windows/System32/cmd.exe\"" "*Win-cmd*"))	     
(defun git-bash-shell ()
  (interactive)
  (let ((buffer (get-buffer "*eshell: Git-bash*")))
    (if buffer
	(switch-to-buffer buffer)
      (progn
	(eshell)
	(rename-buffer "*eshell: Git-bash*")		     
	(with-current-buffer "*eshell: Git-bash*"
	  (insert "/mnt/c/Program\\ Files/Git/bin/bash.exe --login -i | cat")
          (eshell-send-input)
          (comint-clear-buffer)
          (run-at-time "0.2 sec" nil
                       (lambda ()
                         (with-current-buffer "*eshell: Git-bash*"
                           (erase-buffer)))))))))

;==============================================================================
(provide 'wsl-config)
