;; -*- lexical-binding: t -*-
;; WSL Configuration
;;==============================================================================
(defun et-init-wsl (conf stem)
  "Initialize on Windows Subsystem for Linux"
  (et-init-wsl-env)
  (et-init-magit-switch)
  (when (not (display-graphic-p))
    (et-init-wsl-nw-clipboard))
  (use-package sudo-edit
    :bind ("C-c C-r" . 'sudo-edit)))
;;==============================================================================
(defun et-init-wsl-env ()
  (setenv "LOCALHOST"
	  (let ((sout (shell-command-to-string "ip route")))
      	    (progn (string-match "default via \\([0-9.]+\\)" sout)
		   (match-string 1 sout)))))

(defun et-init-wsl-nw-clipboard ()
    (use-package xclip
      :config (xclip-mode 1)))

;;==============================================================================
;; Make Magit useable across the Windows/Linux air-gap

(defun et-init-magit-switch ()
  "Overwrite magit functions with git.exe-from-WSL-friendly alternatives,
and add advice to dynamically adjust the executable before magit execution."
  (with-eval-after-load 'magit
    (fset 'magit-git-string #'et-magit-git-str)
    (fset 'magit-git-str #'et-magit-git-str)
    (fset 'magit-commit-create #'et-magit-commit-create))
  (mapc (lambda (sym) (advice-add sym :before #'et-magit-switch))
	'(magit-status magit-commit magit-push magit-pull magit-fetch)))

(defun et-magit-switch (&optional x &rest _)
  "Set the appropriate Git executable for dynamic default-directory."
  (if (string-prefix-p "/mnt/" default-directory)
      (setq magit-git-executable "/mnt/c/Program Files/Git/bin/git.exe")
    (setq magit-git-executable "/usr/bin/git")))

(defun magit-wsl-with-git-exe-p ()
  "Return non-nil if running in WSL with git executable set to git.exe"
  (and (getenv "WSL_DISTRO_NAME")
       (string-suffix-p "git.exe" magit-git-executable)))

(defun magit-wslpath (path &optional option)
  "Exchange Unix- and Windows-style paths using 'wslpath -option' shell command"
  (if-let* ((_   (getenv "WSL_DISTRO_NAME"))
	    (opt (or option "-u"))
	    (str (shell-command-to-string (format "wslpath %s '%s'" opt path)))
	    (_   (not (string-prefix-p "wslpath: " str))))
      (string-trim str)
    path))

(defun et-magit-git-str (&rest args)
  "Shadows `magit-git-string', but transforms paths if needed."
  (setq args (flatten-tree args))
  (magit--with-refresh-cache (cons default-directory args)
    (magit--with-temp-process-buffer
      (magit-process-git (list t nil) args)
      (unless (bobp)
        (goto-char (point-min))
	(let ((str (buffer-substring-no-properties (point) (line-end-position))))
	  (if (and (magit-wsl-with-git-exe-p)
		   (or (string-match-p "^[a-zA-Z]:[\\/]+.*$" str)
		       (string-match-p "\\.*" str)))
	      (magit-wslpath str)
	    str))))))

(defun et-magit-commit-create (&optional args)
  "Shadows `magit-commit-create', but uses minibuffer instead of Emacsclient."
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (magit-commit-arguments)))
                 (list (magit-commit-arguments))))
  (cond ((member "--all" args)
         (setq this-command 'magit-commit--all))
        ((member "--allow-empty" args)
         (setq this-command 'magit-commit--allow-empty)))
  (when (setq args (magit-commit-assert args))
    (let ((default-directory (magit-toplevel)))
      (magit-run-git "commit" "-m" (read-string "Commit message: ") args))))

;;==============================================================================
;; Misc. Windows utilities inside WSL Emacs
(defun git-bash ()
  "Run Windows-side Git-Bash in terminal emulator."
  (interactive)
  (ansi-term "\"/mnt/c/Program Files/Git/bin/bash.exe\"" "*Git-bash*"))

(defun cmd ()
  "Run CMD in terminal emulator."
  (interactive)
  (ansi-term "\"/mnt/c/Windows/System32/cmd.exe\"" "*Win-cmd*"))

(defun git-bash-shell ()
  "Run Windows-side Git-Bash in eshell. Not great."
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
