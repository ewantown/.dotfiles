;; -*- lexical-binding: t -*-
;; WSL Configuration
;==============================================================================
;; stub
(defun et-init-wsl (conf stem)
  "Initialize on Windows Subsystem for Linux"
  (et-init-wsl-env)
  (if (not (display-graphic-p))
      (et-init-wsl-nw-clipboard))    
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
