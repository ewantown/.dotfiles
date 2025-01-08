;; -*- lexical-binding: t -*-
;; Org Mode configuration
(require 'utils)
;==============================================================================
(defun et-make-org-env (stem)
  (let ((top (concat stem (if (eq system-type 'darwin) "cloud/org/" "org/"))))
    (progn (setq org-directory top)
	   (lambda (key)
	     (let* ((map '((top . "")
			   (docs . "docs/")
			   (time . "time/")
			   (self . "self/")))
		    (dir (cdr (assoc key map)))
		    (path (concat top dir)))
	       (cond ((and dir (file-exists-p path)) path)
		     (dir (make-directory path))
		     (t (error "No matching org directory"))))))))

(defun et-init-org (stem)
  (interactive)
  (let ((org-env (et-make-org-env stem)))
    (define-prefix-command 'et/org-map)
    (global-set-key (kbd "S-<return>") 'open-line)
    (global-set-key (kbd "C-o") 'et/org-map)
    (use-package htmlize)
    (use-package org-superstar)
    (use-package org
      :config
      (et-init-org-time    org-env)
      (et-init-org-babel   org-env)
      (et-init-org-edit    org-env)
      (et-init-org-capture org-env)
      (et-init-org-html-publish stem)
      (setq org-startup-indented t)
      (global-set-key (kbd "M-s-h") 'et-go-home)
      (add-hook 'after-init-hook 'et-go-home)
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
	    ("C-p" . org-previous-visible-heading)
	    ("P" . org-publish-all)))))

(defun et-init-org-time (env)
  (progn
    (setq diary-file (concat (funcall env 'time) ".diary/diary.org"))
    (let ((diarydir (file-name-directory diary-file)))
      (progn
	(unless (file-exists-p diarydir)
	  (make-directory diarydir t))
	(unless (file-exists-p diary-file)
	  (write-region "" nil diary-file)))))
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
	  (append (file-expand-wildcards (concat (funcall env 'docs) "*.org"))
		  (file-expand-wildcards (concat (funcall env 'time) "*.org"))))
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
    ;;		   ("c" . pomidor)))
    ;;   :config (setq pomidor-sound-tick t
    ;;                 pomidor-sound-tack t)
    ;;   :hook (pomidor-mode . (lambda ()
    ;;                           (display-line-numbers-mode -1) ; Emacs 26.1+
    ;;                           (setq left-fringe-width 0 right-fringe-width 0)
    ;;                           (setq left-margin-width 2 right-margin-width 0)
    ;;                           (set-window-buffer nil (current-buffer)))))
    )

(defun et-go-home ()
  (interactive)
  (org-agenda nil "n")
  (delete-other-windows))

(defun et-init-org-capture (env)
  (setq org-capture-templates
      '(("n" "Note" entry
	 (file+datetree (concat (funcall env 'docs) "notes.org"))
	 "* %U\n %?\ncf.: %a"
	 :empty-lines 1)
	("e" "Event" entry
	 (file+headline (concat (funcall env 'time) "events.org") "Calendar")
	 "* %^T %^{Event}"
	 :empty-lines 1)
	("m" "Meeting" entry
	 (file+headline (concat (funcall env 'time) "events.org") "Meetings")
	 "** %^T Meet with %^{With} about %^{About}\n*** Notes:\n%?"
	 :empty-lines 1)
	("t" "Task" entry
	 (file+headline (concat (funcall env 'time) "tasks.org") "Tasks")
	 "* { } [#%^{Priority}] %?%i"
	 :empty-lines 1)
	("p" "Project" entry
	 (file (concat (funcall env 'docs) "make.org"))
	 "* %^{Headline}\nDEADLINE: %^t\n** Summary:\n%?\n** Notes\n"
	 :empty-lines 1)
	("l" "Log" entry
	 (file+datetree+prompt (concat (funcall env 'time) "log.org"))
	 "* %T%i"
	 :empty-lines 1)
	("w" "Weigh-in" entry
	 (file+headline (concat (funcall env 'self) "diet.org") "Logs")
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

(defun et-init-org-babel (env)
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

(defun et-init-org-edit (env)
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
  (setq org-cite-global-bibliography (list (concat (funcall env 'docs) "/lib.bib")))
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

;===============================================================================
(defun et-init-org-html-publish (&optional stem)
  (interactive)  
  (let ((stem (or stem STEM)))
    (use-package nice-org-html
      :ensure nil ; use local repo for dev
      :hook (org-mode . nice-org-html-mode)
      :config
      (setq org-html-validation-link nil)
      (setq org-publish-timestamp-directory
	    (concat user-emacs-directory ".org-timestamps/"))
      (setq org-publish-use-timestamps-flag nil)
      (setq nice-org-html-theme-alist
            '((dark . tomorrow-night-eighties) (light . solo-jazz)))
      (setq org-publish-project-alist
	    (et-get-org-publish-project-alist stem))
      )))

(defun et-nice-org-html-sample-project (stem id light-thm dark-thm)
  (let* ((basedir
	  (concat stem "local/repos/nice-org-html/docs/samples/source/"))
	 (pubdir
	  (concat stem "local/repos/nice-org-html/docs/samples/" id "/"))
	 (header
          '(("SITE NAME" . "/route/to/home.html")
            ("Foo" . "/route/to/foo.html")
            ("Bar" . "/route/to/bar.html")
            ("Baz" . "/route/to/baz.html")))
	 (footer 
          '(("© Ewan Townshend" . "mailto:e@etown.dev")
            ("GitHub" . "https://github.com/ewantown")
            ("LinkedIn" . "https://linkedin.com/in/ewan-townshend")))
	 (theme-alist (list (cons 'light light-thm) (cons 'dark dark-thm)))
	 (default-mode 'dark)
	 (headline-bullets (list :h1 "1" :h2 "2" :h3 "3" :h4 "4" :h5 "5"))
	 (pubfn
	  (nice-org-html-make-publishing-function
	   `((light . ,light-thm) (dark . ,dark-thm))
	   'dark
	   nil header footer "" ""
	   '(:collapsing t))))
    `(,(format "nice-org-html/sample/%s" id)
      :base-directory ,basedir
      :base-extension "org"
      :publishing-directory ,pubdir      
      :htmlized-source t
      :headline-levels 5
      :section-numbers nil
      :with-entities t
      :with-latex t
      :with-toc t
      :with-author nil
      :with-creator nil
      :with-date nil
      :with-email nil
      :time-stamp-file nil
      :publishing-function ,pubfn
      )))

(defun et-get-org-publish-project-alist (stem)
  (match system-type
	 ('darwin
	  `(
	    ("nice-org-html/readme"
	     :base-directory
	     ,(concat stem "local/repos/nice-org-html/docs/")	     
	     :publishing-directory
	     ,(concat stem "local/repos/nice-org-html/docs/")
	     :base-extension "org"
	     :recursive nil
	     :htmlized-source t
	     :headline-levels 3
	     :section-numbers nil
	     :with-entities t
	     :with-latex t
	     :with-toc t
	     :with-author nil
	     :with-creator nil
	     :with-date nil
	     :with-email nil
	     :time-stamp-file nil
	     :publishing-function	     
	     ,(nice-org-html-publishing-function
	       :theme-alist
	       ((light . solarized-light) (dark . tomorrow-night-eighties))
	       :default-mode dark
	       :headline-bullets (:h1 "" :h2 "▷" :h3 "" :h4 "" :h5 ""))
	     )
	    ,(et-nice-org-html-sample-project
	       stem "tsdh" 'tsdh-light 'tsdh-dark)
	    ,(et-nice-org-html-sample-project
	      stem "ample-zenburn" 'ample-light 'zenburn)
	    ,(et-nice-org-html-sample-project
	      stem "eighties-jazz" 'solo-jazz 'tomorrow-night-eighties)
	    ,(et-nice-org-html-sample-project
	      stem "tomorrow" 'tomorrow-day 'tomorrow-night)
	    ,(et-nice-org-html-sample-project
	      stem "solarized" 'solarized-light 'solarized-dark)
	    ,(et-nice-org-html-sample-project
	      stem "spacemacs" 'spacemacs-light 'spacemacs-dark)
	    ,(et-nice-org-html-sample-project
	      stem "leuven" 'leuven 'leuven-dark)
	    ,(et-nice-org-html-sample-project
	      stem "modus" 'modus-operandi 'modus-vivendi)
	    ("nice-org-html/docs"
	     :components ("nice-org-html/readme"
			  "nice-org-html/sample/tsdh"
			  "nice-org-html/sample/ample-zenburn"
			  "nice-org-html/sample/eighties-jazz"
			  "nice-org-html/sample/tomorrow"
			  "nice-org-html/sample/solarized"
			  "nice-org-html/sample/spacemacs"
			  "nice-org-html/sample/leuven"
			  "nice-org-html/sample/modus")
	     )
	    ("etown.dev/files"
	     :base-directory ,(concat stem "local/repos/etown.dev/org/")
	     :base-extension "org"
	     :publishing-directory ,(concat stem "local/repos/etown.dev/site/")
	     :htmlized-source t
	     :recursive t
	     :auto-sitemap t
	     :sitemap-title "Sitemap"
	     :headline-levels 3
	     :section-numbers nil
	     :with-entities t
	     :with-latex t
	     :with-toc t
	     :with-author nil
	     :with-creator nil
	     :with-date nil
	     :with-email nil
	     :time-stamp-file nil
	     :publishing-function
	     ,(nice-org-html-publishing-function
	       :theme-alist
	       ((dark . spacemacs-dark) (light . spacemacs-light))
	       :default-mode dark
	       :headline-bullets (:h1 "" :h2 "" :h3 "▷" :h4 "" :h5 "")
	       :header
	       (("Ewan Townshend" . "home")
		("About" . "about")
		("Projects" . "projects")
		("Thoughts" . "thoughts"))
	       :footer
	       (("© 2025" . nil)
		("Email" . "mailto:ewan@etown.dev")
		("LinkedIn" . "https://www.linkedin.com/in/ewan-townshend")
		("Resumé" . "file:./other/sw-resume.pdf")))
	     )
	    ("etown.dev/images"
	     :base-directory ,(concat stem "local/repos/etown.dev/org/images/")
	     :base-extension "jpg\\|gif\\|png"
	     :publishing-directory ,(concat stem "local/repos/etown.dev/site/images/")
	     :publishing-function org-publish-attachment)
	    ("etown.dev/other"
	     :base-directory ,(concat stem "local/repos/etown.dev/org/other/")
	     :base-extension "./"
	     :publishing-directory ,(concat stem "local/repos/etown.dev/site/other/")
	     :publishing-function org-publish-attachment)
	    ("etown.dev"
	     :components ("etown.dev/files" "etown.dev/images" "etown.dev/other"))
	    ))
	 ('gnu/linux  '())
	 ('windows-nt '())))

;==============================================================================
(provide 'org-config)
