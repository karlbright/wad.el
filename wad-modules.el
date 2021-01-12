;;; wad-packages.el -*- lexical-binding: t; -*-

(defvar wad/modules (make-hash-table :test 'equal)
  "A table of enabled modules. Set by `wad/modules!' where the key
is a cons of (category . module) and the value is a plist of metadata
related to the module.")

(defvar wad/packages-file "packages"
  "The basename of packages file for modules.")

(defun wad/module-log (category module format-string &rest args)
  (let* ((category (wad/keyword-to-string category))
	 (module (symbol-name module))
	(prefix (concat "[" category "/" module "]")))
    (wad/log (concat prefix " " format-string args))))

(defun wad/modules-list-map (fn list)
  "Apply FN to each module in LIST."
  (let ((modules (copy-sequence list))
	results category curr)
    (while modules
      (setq curr (pop modules))
      (cond ((keywordp curr)
	     (setq category curr))
	    ((null category)
	     (error "No module category specified for %s" curr))
	    (t (let ((module (if (listp curr) (car curr) curr))
		     (flags (if (listp curr) (cdr curr))))
		 (push (funcall fn category module
				:flags flags
				:path (wad/module-locate-path category module))
		       results)))))
    (nreverse results)))

(defun wad/modules-reset! ()
  "Resets modules to initial state."
  (clrhash wad/modules))

(defun wad/module-p (category module)
  "Returns t if CATEGORY MODULE is enabled."
  (declare (pure t) (side-effect-free t))
  (when-let (plist (gethash (cons category module) wad/modules))
    t))

(defun wad/module-from-current-path ()
  (wad/module-from-path (wad/current-file!)))

(defun wad/module-from-path (path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH."
  (if wad/current-module
      wad/current-module
    (let* ((file-name-handler-alist nil)
	   (path (file-truename path)))
      (save-match-data
	(cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
	       (when-let* ((category (wad/string-to-keyword (match-string 1 path)))
			   (module (wad/string-to-symbol (match-string 2 path))))
		 (if (wad/module-p category module)
		     (cons category module)))))))))

(defun wad/module-locate-path (category &optional module file)
  "Searches `wad/modules-dirs' to find the path to a module.

CATEGORY is a keyword and MODULE is a symbol. FILE is a string
that will be appended to the resulting path. If no path exists,
this returns nil, otherwise an absolute path.

This doesn't require the module to be enabled."
  (when (keywordp category)
    (setq category (wad/keyword-to-string category)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop with file-name-handler-alist = nil
           for default-directory in wad/modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (file-truename path)))

(defun wad/module-set (category module &rest plist)
  "Enables a module by adding it to `wad/modules'.

CATEGORY is a keyword, MODULE is a symbol, PLIST is a plist that
accepts the following properties:

  :flags [SYMBOL LIST] list of enabled category flags
  :path  [STRING]      path to category root directory."
  (puthash (cons category module) plist wad/modules))

(defun wad/module-get (category module)
  "Get plist of module from `wad/modules' matching CATEGORY and MODULE.

CATEGORY is a keyword, MODULE is a symbol."
  (gethash (cons category module) wad/modules))

(defun wad/module-loader (file)
  "Return a closure that loads FILE from module.

The closure takes two arguments: a cons cell containing (CATEGORY . MODULE)
symbols, and the matching module plist."
  (declare (pure t) (side-effect-free t))
  (lambda (module plist)
    (let ((wad/current-module module)
	  (wad/current-flags (plist-get plist :flags)))
      (wad/load! file (plist-get plist :path) t))))

(defun wad/modules-map (fn &optional modules)
  "Apply FN to each module in `wad/modules'.

If MODULES is provided, will only apply FN to modules whose (CATEGORY . NAME)
is a member of MODULES. For example:

(wad/modules-map #'my-module-fn '((:core . foobar) (:tools . magit)))"
  (maphash
   (lambda (module plist)
     (if (or (null modules) (and modules (member module modules)))
	 (let ((category (car module))
	       (module (cdr module)))
	   (funcall fn category module plist))))
   wad/modules))

(defun wad/initialize-module (category module &optional plist)
  "Load module init.el for module with CATEGORY and MODULE."
  (wad/module-log category module "wad/initialize-module")
  (funcall (wad/module-loader wad/module-init-file)
	   (cons category module)
	   (or plist (wad/module-get category module))))

(defun wad/initialize-modules (&optional modules)
  "Initialize modules."
  (wad/modules-map #'wad/initialize-module modules))

(defun wad/initialize-module-packages (category module &optional plist)
  "Loads `wad/module-packages-file' for given module with CATEGORY and MODULE."
  (wad/module-log category module "wad/initialize-module-packages")
  (funcall (wad/module-loader wad/module-packages-file)
	   (cons category module)
	   plist))

(wad/initialize-module-packages :core 'straight)

(defun wad/configure-module (category module &optional plist)
  "Load module config.el for module with CATEGORY and MODULE."
  (wad/module-log category module "wad/configure-module")
  (funcall (wad/module-loader wad/module-config-file)
	   (cons category module)
	   (or plist (wad/module-get category module))))

(defmacro wad/modules! (&rest modules)
  "Bootstraps modules and populate `wad/modules'."
  `(progn
     (wad/modules-list-map
      (lambda (category module &rest plist)
	(if (plist-get plist :path)
	    (apply #'wad/module-set category module plist)
	  (message "Could not find module for %s/%s" category module)))
      ,@(if (keywordp (car modules))
	    (list (list 'quote modules))
	  modules))
     wad/modules))

(defun wad/packages-for-module-map (fn &optional module)
  "Apply FN to each package in `wad/packages'.

If MODULE is provided, will only apply FN to package whose :modules includes
MODULE. For example:

(wad/packages-for-module-map #'install-package '(:core . straight))"
  (cl-loop for package in wad/packages
	   for name = (car package)
	   for modules = (wad/package-get name :modules)
	   when (if module (member module modules) t)
	   do (funcall fn name package)))

(defun wad/ensure-module-packages (category module &optional plist)
  "Ensures all packages are installed found in module matching MODULE,
where MODULE is (CATEGORY . NAME) cons cell. See `wad/modules' for more
information."
  (wad/packages-for-module-map
   (lambda (name &rest _)
     (wad/ensure-package name))
   (cons category module)))

(defun wad/ensure-modules-packages (&optional modules)
  "Initialize module and ensures it's packages are installed."
  (wad/modules-map #'wad/ensure-module-packages modules))

(provide 'wad-modules)
