;; Patches
;==============================================================================
(require 'utils)

(defun patch ()
  "Run patches for this (major) version of Emacs"
  (patch-packages)
  (match emacs-major-version
	 (28 (patch-28))
	 (29 (patch-29))
	 ))

(defun patch-28 ()
  `(defun image-type-available-p (type)     
     "Return t if image type TYPE is available. TYPE is symbol; e.g. `xbm' or `jpeg'."
     (if (eq 'svg type)
	 nil
       (and (fboundp 'init-image-library)
            (init-image-library type)))))

(defun patch-29 ()  
  "No patches for Emacs 29"
  `(message "No patches for Emacs 29"))

(defun patch-packages ()
  (eval-after-load 'ox-publish (lambda () (patch-ox-publish))))

(defun patch-ox-publish ()
  (defun org-publish-write-cache-file (&optional free-cache)
    "Write `org-publish-cache' to file.
If FREE-CACHE, empty the cache."
    (unless org-publish-cache
      (error "`org-publish-write-cache-file' called, but no cache present"))
    
    (let ((cache-file (org-publish-cache-get ":cache-file:")))
      (unless cache-file
	(error "Cannot find cache-file name in `org-publish-write-cache-file'"))
      (unless (file-exists-p cache-file) (make-empty-file cache-file)); ET ADDED
      (with-temp-file cache-file
	(let (print-level print-length)
	  (insert "(setq org-publish-cache \
\(make-hash-table :test 'equal :weakness nil :size 100))\n")
	  (maphash (lambda (k v)
		     (insert
		      (format "(puthash %S %s%S org-publish-cache)\n"
			      k (if (or (listp v) (symbolp v)) "'" "") v)))
		   org-publish-cache)))
      (when free-cache (org-publish-reset-cache)))))

;==============================================================================
(provide 'patches)
