;; -*- lexical-binding: t -*-
;; WSL Configuration
;==============================================================================
;; stub
(defun et-init-wsl (conf stem)
  "Initialize on Windows Subsystem for Linux"
  (et-init-wsl-env)
  (et-init-db))
;==============================================================================
(defun et-init-wsl-env ()
  (setenv "LOCALHOST"
	  (let ((sout (shell-command-to-string "ip route")))
      	    (progn (string-match "default via \\([0-9.]+\\)" sout)
		   (match-string 1 sout)))))

(defun et-init-db ()
  (use-package sql
    :config
    (setq sql-db2-program "/mnt/d/SQLLIB/BIN/db2cmd.exe"
	  sql-db2-options '("-c" "-i" "-w" "db2" "-tv")
	  sql-user (getenv "SQL_USER")
	  sql-password (getenv "SQL_PASS"))
    (defalias 'sql-get-login 'ignore) ; login with comint buffer
    (advice-add 'sql-send-paragraph :before 'cms/db-connect)))

(defun et-db-connect ()
  (interactive)
  (unless (sql-buffer-live-p sql-buffer)
    (let ((db (read-string "DB Instance: ")))
      (let ((new (not (get-buffer (concat "*SQL: " db "*"))))
	    (window (selected-window)))
	(progn
	  (sql-db2 (concat "*SQL: " db "*"))
	  (if new
	      (progn
		(switch-to-buffer (concat "*SQL: " db "*"))
		(toggle-truncate-lines 1)
		(comint-clear-buffer)
		(insert
		 (format (concat "connect to %s user " sql-user " using " sql-password ";") db))
		(comint-send-input)))
	  (select-window window))))))

;==============================================================================
(provide 'wsl-config)
