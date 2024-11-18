;; Patches
;==============================================================================
(require 'utils)

(defun patch ()
  "Run patches for this (major) version of Emacs"
  (match emacs-major-version
	 (28 (patch-28))
	 (29 (patch-29))
	 ))

(defmacro patch-28 ()
  `(defun image-type-available-p (type)     
     "Return t if image type TYPE is available. TYPE is symbol; e.g. `xbm' or `jpeg'."
     (if (eq 'svg type)
	 nil
       (and (fboundp 'init-image-library)
            (init-image-library type)))))

(defmacro patch-29 ()  
  "No patches for Emacs 29"
  `(message "No patches for Emacs 29"))

(provide 'patches)
