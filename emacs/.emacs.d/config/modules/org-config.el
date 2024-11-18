;; -*- lexical-binding: t -*-
;; Org Mode configuration
(require 'utils)
;==============================================================================
(defun make-org-env (stem)  
  (let ((top (concat stem (if (eq system-type 'darwin) "cloud/org/" "org/"))))
    (progn (setq org-directory top)
	   (lambda (key)
	     (let* ((map '((docs . "docs/") (time . "time/") (self . "self/")))
		    (dir (cdr (assoc key map)))
		    (path (concat top dir)))
	       (cond ((and dir (file-exists-p path)) path)
		     (dir (make-directory path))
		     (t (error "No matching org directory"))))))))

(defun init-org (stem)
  (let ((org-env (make-org-env stem)))
    (interactive)
    (define-prefix-command 'et/org-map)
    (global-set-key (kbd "S-<return>") 'open-line)
    (global-set-key (kbd "C-o") 'et/org-map)
    (use-package org-superstar)
    (use-package org      
      :config
      (setup-org-time    org-env)
      (setup-org-babel   org-env)
      (setup-org-edit    org-env)
      (setup-org-capture org-env)
      (setq org-startup-indented t)
      (add-hook 'after-init-hook 'et-go-home)
      (global-set-key (kbd "M-s-h") 'et-go-home)
      (with-eval-after-load 'org-agenda
	(progn
	  (define-key org-agenda-mode-map (kbd "S-<left>") nil)
	  (define-key org-agenda-mode-map (kbd "S-<right>") nil)
	  (define-key org-agenda-mode-map (kbd "S-<up>") nil)
	  (define-key org-agenda-mode-map (kbd "S-<down>") nil)))
      :hook (org-mode . (lambda ()
			  (visual-line-mode)
			  (org-superstar-mode)))
      :bind
      (:map et/org-map
            ("M-<return>" . org-babel-execute-src-block)
            ("t" . org-insert-structure-template)
            ("<return>" . gptel-send)
            ("s" . org-store-link)
            ("i" . org-insert-link)
            ("c" . org-capture)
            ("a" . org-agenda)
            ("A" . et/go-home)
            ("r" . org-cite-insert)
            ("C-F" . org-footnote-new)
            ("f" . org-footnote-action)
            ("C-a" . et/chatgpt-map)
            ("s" . org-store-link)
            ("C-n" . org-next-visible-heading)
            ("C-p" . org-previous-visible-heading)))))

(defun setup-org-time (env)
  (progn
    (setq diary-file (concat (: env 'time) ".diary/diary.org"))
    (setq calendar-date-style 'iso
          diary-show-holidays-flag nil
          calendar-mark-diary-entries-flag t
          holiday-islamic-holidays nil
          holiday-hebrew-holidays nil
          holiday-bahai-holidays nil
          holiday-christian-holidays nil
          holiday-oriental-holidays nil
          holiday-other-holidays nil)
    (setq org-todo-keywords
          '((sequence "TODO" "DOIN" "|" "DONE")
	    (sequence "{ }" "{~}" "|" "{*}")
	    (sequence "{-}" "|" "{+}")
	    (sequence "{?}" "|" "{*}")))
    (setq org-todo-keyword-faces
          '(("TODO" . (:foreground "#f67c8b" :weight bold))
            ("DOIN" . (:foreground "#fb91fb" :weight bold))
            ("DONE" . (:foreground "#9cfdcd" :weight bold))
            ("{ }" . (:foreground "#f67c8b" :weight bold))
            ("{-}" . (:foreground "#f67c8b" :weight bold))
            ("{~}" . (:foreground "#fb91fb" :weight bold))
            ("{?}" . (:foreground "#ff7f00" :weight bold))
            ("{*}" . (:foreground "#9cfdcd" :weight bold))))
    (setq org-agenda-include-diary t)
    (setq org-agenda-files
          (append (file-expand-wildcards (concat (: env 'docs) "*.org"))
                  (file-expand-wildcards (concat (: env 'time) "*.org"))))
    (setq org-agenda-prefix-format
	  '((agenda . " %?-12t% s ")
            (todo . " %i ");%?-12:c")
            (tags . " %i %-12:c")
            (search . " %i %-12:c")))
    (setq org-agenda-custom-commands
        '(("n" "Now" ; "homepage"
	       ((agenda "" ((org-agenda-span 1)
		                (org-agenda-overriding-header "TODAY:")
		                (org-agenda-skip-function
		                 '(org-agenda-skip-entry-if
			               'todo
		      	           '("TODO" "DOIN" "{ }" "{~}")))
		                (org-deadline-warning-days 1)
		                (org-deadline-past-days 1)))
	        (todo "TODO|DOIN|{ }|{~}|{?}"
                  ((org-agenda-overriding-header "TASKS:")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":routine:"))))
	        (agenda "" ((org-agenda-overriding-header "SOON:\n")
		                (org-agenda-start-day "+1d")
		                (org-agenda-span 10)
		                (tags "+CATEGORY=\"Event\"" "-CATEGORY=\"Cyclic\"")
		                (org-agenda-skip-function
		                 '(org-agenda-skip-entry-if
			               'todo
			               '("TODO" "DOIN" "DONE" "{-}" "{ }")
                           'regexp ":routine:"))))))))
    ;; (use-package pomidor
    ;;   :bind ((:map et/org-map
    ;; 		   ("c" . pomidor)))
    ;;   :config (setq pomidor-sound-tick t
    ;;                 pomidor-sound-tack t)
    ;;   :hook (pomidor-mode . (lambda ()
    ;;                           (display-line-numbers-mode -1) ; Emacs 26.1+
    ;;                           (setq left-fringe-width 0 right-fringe-width 0)
    ;;                           (setq left-margin-width 2 right-margin-width 0)
    ;;                           (set-window-buffer nil (current-buffer)))))
    ))

(defun et-go-home ()
  (interactive)
  (org-agenda nil "n")
  (delete-other-windows))

(defun setup-org-capture (env)
  (setq org-capture-templates
      '(("n" "Note" entry
	 (file+datetree (concat (: env 'docs) "notes.org"))
         "* %U\n %?\ncf.: %a"
	 :empty-lines 1)
	("e" "Event" entry
	 (file+headline (concat (: env 'time) "events.org") "Calendar")
         "* %^T %^{Event}"
	 :empty-lines 1)
	("m" "Meeting" entry
	 (file+headline (concat (: env 'time) "events.org") "Meetings")
         "** %^T Meet with %^{With} about %^{About}\n*** Notes:\n%?"
	 :empty-lines 1)
	("t" "Task" entry
	 (file+headline (concat (: env 'time) "tasks.org") "Tasks")
         "* { } [#%^{Priority}] %?%i"
	 :empty-lines 1)
	("p" "Project" entry
	 (file (concat (: env 'docs) "make.org"))
         "* %^{Headline}\nDEADLINE: %^t\n** Summary:\n%?\n** Notes\n"
	 :empty-lines 1)
	("l" "Log" entry
	 (file+datetree+prompt (concat (: env 'time) "log.org"))
         "* %T%i"
	 :empty-lines 1)
	("w" "Weigh-in" entry
	 (file+headline (concat (: env 'self) "diet.org") "Logs")
         "** { } %t
%^{Weight}p
|-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
| Time  | Food | Quant | cals/u | fats/u | carb/u | prot/u | Cals | Fats | Carbs | Prot |
|-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
|       |      |       |        |        |        |        |      |      |       |      |
|-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
| Total |      |       |        |        |        |        |      |      |       |      |
|-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
#+TBLFM: @2$8..@>>$8=$3*$4::@2$9..@>>$9=$3*$5::@2$10..@>>$10=$3*$6::@2$11..@>>$11=$3*$7
#+TBLFM: @>$8=vsum(@2$8..@-I$8)::@>$9=vsum(@2$9..@-I$9)
#+TBLFM: @>$10=vsum(@2$10..@-I$10)::@>$11=vsum(@2$11..@-I$11)"
:empty-lines 1))))

(defvar weigh-in-data
  ;%^{Weight}p
  "
   |-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
   | Time  | Food | Quant | cals/u | fats/u | carb/u | prot/u | Cals | Fats | Carbs | Prot |
   |-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
   |       |      |       |        |        |        |        |      |      |       |      |
   |-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
   | Total |      |       |        |        |        |        |      |      |       |      |
   |-------+------+-------+--------+--------+--------+--------+------+------+-------+------|
   #+TBLFM: @2$8..@>>$8=$3*$4::@2$9..@>>$9=$3*$5::@2$10..@>>$10=$3*$6::@2$11..@>>$11=$3*$7
   #+TBLFM: @>$8=vsum(@2$8..@-I$8)::@>$9=vsum(@2$9..@-I$9)
   #+TBLFM: @>$10=vsum(@2$10..@-I$10)::@>$11=vsum(@2$11..@-I$11)")

(defun setup-org-babel (env)
  ;;; Babel
  ;(add-to-list 'load-path "~/.emacs.d/config/packages/ob-racket.el")
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-clojure-backend 'cider)  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     ;(chez .t)
     ;(racket . t)
     (scheme . t)
     (java . t))))
  ;(setq org-babel-command:racket "/opt/homebrew/bin/racket"))

(defun setup-org-edit (env)
  (visual-line-mode)
  (org-indent-mode)
  ;(setq org-hide-emphasis-markers nil)
  (setq org-adapt-indentation 'headline-data)
  ;(auto-fill-mode)
  (prettify-symbols-mode)
  (setq org-pretty-entities nil)                                
  (setq org-return-follows-link  t)
  (setq org-cycle-separator-lines 1)
  (setq org-footnote-auto-adjust 't)
  (setq org-cite-global-bibliography (list (concat (: env 'docs) "/lib.bib")))
  (local-set-key (kbd "s-<return>") 'org-tree-to-indirect-buffer)  
  (setq org-preview-latex-process-alist
	'((dvipng :programs
		 ("latex" "dvipng")
		 :description "dvi > png"
		 :message "you need to install the programs: latex and dvipng."
		 :image-input-type "dvi"
		 :image-output-type "png"
		 :image-size-adjust (1.0 . 1.0)		 
		 :latex-compiler
		 ("latex -interaction nonstopmode -output-directory %o %f")
		 :image-converter
		 ("dvipng -D %D -T tight -o %O %F")
		 :transparent-image-converter
		 ("dvipng -D %D -T tight -bg Transparent -o %O %F"))
	 (dvisvgm :programs
		  ("latex" "dvisvgm")
		  :description "dvi > svg"
		  :message "you need to install the programs: latex and dvisvgm."
		  :image-input-type "dvi"
		  :image-output-type "svg"
		  :image-size-adjust (1.7 . 1.5)		  
		  :latex-compiler
		  ("latex -interaction nonstopmode -output-directory %o %f")
		  :image-converter
		  ("dvisvgm %F --no-fonts --exact-bbox --scale=%S --output=%O"))
	 (imagemagick :programs
		      ("latex" "convert")
		      :description "pdf > png"
		      :message "you need to install the programs: latex and imagemagick."
		      :image-input-type "pdf"
		      :image-output-type "png"
		      :image-size-adjust (1.0 . 1.0)		      
		      :latex-compiler
		      ("pdflatex -interaction nonstopmode -output-directory %o %f")
		      :image-converter
		      ("convert -density %D -trim -antialias %F -quality 100 %O"))))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-entities-user '(("proves" "\\vdash" t "[...]" "[...]" "[...]" "⊢")
			    ("notproves" "\\neg \\vdash" t "[...]" "[...]" "[...]" "⊬")
			    ("models" "\\models" t "[...]" "[...]" "[...]" "⊨")
			    ("notmodels" "\\neg \\models" t "[...]" "[...]" "[...]" "⊭")
			    ("forces" "\\Vdash" t "[...]" "[...]" "[...]" "⊩")
			    ("notforces" "\\neg \\Vdash" t "[...]" "[...]" "[...]" "⊮")
			    ("Implies" "\\Rightarrow" t "[...]" "[...]" "[...]" "⟹")
			    ("Iff" "\\Leftrightarrow" t "[...]" "[...]" "[...]" "⟺")
			    ("mapsto" "\\mapsto" t "[...]" "[...]" "[...]" "↦")
			    ("rcrow" "" t "[...]" "[...]" "[...]" "⤙")
			    ("lcrow" "" t "[...]" "[...]" "[...]" "⤚")
			    ("natjoin" "" t "\\bowtie" "[...]" "[...]" "⋈")
			    ("lsemijoin" "" t "[...]" "[...]" "[...]" "⋉")
			    ("rsemijoin" "" t "[...]" "[...]" "[...]" "⋊")
			    ("antijoin" "" t "[...]" "[...]" "[...]" "▷")
			    ("outerjoin" "" t "[...]" "[...]" "[...]" "⟗")
			    ("louterjoin" "" t "[...]" "[...]" "[...]" "⟕")
			    ("routerjoin" "" t "[...]" "[...]" "[...]" "⟖")))
  (use-package citar
    :config
    (setq org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar
	  ;(setq org-cite-insert-processors '((latex . oc-bibtex)))
	  ;(setq org-cite-export-processors '((latex . biblatex)))
          citar-bibliography org-cite-global-bibliography)))

;==============================================================================
(provide 'org-config)