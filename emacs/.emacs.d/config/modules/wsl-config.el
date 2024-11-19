;; -*- lexical-binding: t -*-
;; WSL Configuration
;==============================================================================
;; stub
(defun et-init-wsl (conf stem)
  "Initialize on Windows Subsystem for Linux"
  (et-init-wsl-env))
;==============================================================================
(defun et-init-wsl-env ()
  (setenv "LOCALHOST"
	  (let ((sout (shell-command-to-string "ip route")))
      	    (progn (string-match "default via \\([0-9.]+\\)" sout)
		   (match-string 1 sout)))))

;==============================================================================
(provide 'wsl-config)
