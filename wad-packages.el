;;; wad-packages.el -*- lexical-binding: t; -*-

;;; State

(defvar wad/packages '()
  "A list of enabled packages.

Each element is a sublist, whose CAR is the
package's name as a symbol, and whose CDR is the plist supplied to it's
`package!' declaration.

Set by `wad/init-packages'.")

(defun wad/package-get (name &optional property)
  "Returns package plist matching NAME. Optionally, if a PROPERTY has
been provided, and is a member of the package plist, it will be returned.

Returns nil if the package does not exist, or the property does not
exist on the package plist, if provided."
  (let ((plist (cdr (assq name wad/packages))))
    (if property
	(plist-get plist property)
      plist)))

(defun wad/package-add (name plist)
  "Add package to `wad/packages', replacing any existing package."
  (setf (alist-get name wad/packages)
	(if (listp plist) plist (list plist))))

(defun wad/packages-reset! ()
  "Resets wad/packages to initial state."
  (setq wad/packages nil))

(defun wad/ensure-package (name)
  "Ensures package is installed, previously declare using `wad/package!' by NAME."
  (let* ((package (wad/package-get name))
	 (recipe (plist-get package :recipe))
	 (fn (if (functionp recipe)
		 recipe
	       (or #'wad/ensure-package-handler
		   (lambda (&rest _) (wad/log "Missing 'wad/ensure-package-handler'".))))))
    (if package (funcall fn name package))
    nil))

(defun wad/package-recipe-keys-p (plist)
  "Basic key validation. Throws error on invalid properties."
  (condition-case e
    (cl-destructuring-bind (&key _host _repo _branch) plist t)
  (error nil)))

(defmacro wad/package! (name &rest plist &keys recipe)
  "Declare a package and how to install it (if applicable)."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (plist-put! plist :recipe `(quote ,recipe)))
  `(let* ((name ',name)
	  (existing-plist (wad/package-get name))
	  (result (copy-sequence existing-plist))
	  (modules (wad/package-get name :modules))
	  (module ',(wad/module-from-current-path)))
     (unless (member module modules)
        (wad/plist-put! result :modules
		       (append modules (list module))))
     (wad/doplist! ((prop val) (list ,@plist) result)
       (unless (null val) (wad/plist-put! result prop val)))
     (wad/log "Adding package %s to wad/packages" name)
     (wad/package-add name result)
     wad/packages))

(defun wad/ensure-package-handler (name package))

(provide 'wad-packages)
