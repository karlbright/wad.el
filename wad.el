;;; wad.el --- Bring your own package managers -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright (C) 2019-2021 Karl Brightman
;; Author: Karl Brightman <git@karlbrig.ht>
;; Homepage: http://sr.ht/karlbright/wad.el
;; Package-Requires: ((emacs "25.0"))
;; Version: 0.2.0

;;; Commentary:

;; TODO

;;; Code:

(require 'cl-lib)

;;;; Security

(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 2048)
(setq network-security-level 'high)
(setq nsm-save-host-names t)
      
;;;; Library State

(defvar wad--debug-p (or (getenv-internal "DEBUG") init-file-debug)
  "Non-nil if currently in debug mode for wad.el")

(defvar wad--modules-directories ()
  "A list of directories to search for modules in. Ordered by priority.")

;;;; Base error

(define-error 'wad--error "wad.el error")

;;;; User

(defmacro wad! (&rest modules)
  "Bootstrap wad.el modules.
If the first item in MODULES doesn't satisfy `keywordp' function, MODULES is
evaluated. Otherwise, MODULES is a multiplayer list.
The bootstrap process involves:
 - Adding each module to `wad--modules', replacing any existing module matching
   the same name.
 - Check if a module has a `wad--module-package-file' provided. If so, then
   evaluate declarations provided. Linking the packages to the modules they
   were defined in.
   This ensures that future modules can use predicate functions correctly.
 - Initialise the modules in the order they were provided to `wad!'. This ensures that
   any predicate used in other modules will be correctly evaluated based on it's
   previous modules etc. Note, that if a predicate is used as part of the module
   `wad--module-init-file', that it should not assume a package is installed at this
   point.
 - Ensure that the packages declared are correctly installed and available to the
   modules and user.
 - Finally, if a module has a provided `config.el', then ensure we load an evaluate
   the file for any module configuration.
Module load order is determined by your `wad!' block. Order defines precedence, from
most to least."
  (cl-flet
      ((wad--map
       (fn modules)
       (let ((modules (copy-sequence modules)) result category ocurr)
	 (while modules
	   (setq curr (pop modules))
	   (cond
	    ((keywordp curr) (setq category curr))
	    ((null category) (wad--error-missing-category curr)))
	   (when-let* ((module curr)
		       (path (wad--module-path-from-key category module)))
	     (push (funcall fn category module :flags nil :path path) result)
	     (nreverse result))))))
    `(progn
       (run-hook-wrapped 'wad--hook-before #'wad--hook-try-run)
       (wad--map
	(lambda (category module &rest plist)
	  (if (plist-member plist :path)
	      (apply #'wad--module-set category module plist)
	    (wad--log "Skipping missing module %s/%s" category module)))
	,@(if (keywordp (car modules)) (list (list 'quote modules))
	    (wad--ensure-list modules)))
       (wad--module-step! init)
       (wad--module-step! init-packages)
       (wad--module-step! ensure-packages)
       (wad--module-step! config)
       (run-hook-wrapped 'wad--hook-after #'wad--hook-try-run))))

(cl-defmacro wad-package! (name &rest plist
				&key type recipe ignore disable pin)
  "Declares a package and optionally, how to install it.
This macro is declarative and does not load nor install packages. It is used to
populate `wad-packages' with metadata about the packages defined by the users
enabled modules. You should only be using this macro within a module file marked
by `wad--packages-file'.
Accepts the following properties:
 :type built-in|virtual
  Specifies what kind of package this is. Can be a symbol or a list thereof.
  `built-in' = this package is already built-in and does not need installing.
  `virtual' = should not be tracked by wad.el and will not be installed.
 :recipe RECIPE|FUNCTION
  Specifies a recipe for how to acquire package from external sources. There are
  no requirements on what the RECIPE should or should not be. The contents of
  the recipe will differ depending on which package manager is being used. When
  RECIPE is a function, the function will be run and it is assumed that the
  function should install the package correctly. This should not be used outside
  of installed a package manager to handle future :recipe props.
 :disable BOOL
  Will not attempt to install this package and disable it's `wad-use!' and `after!'
  blocks.
 :ignore FORM
  Do not install this package if the `:ignore' keyword is present without a form.
  If a form is provided, then will ignore the package if the form evaluated to
  non-nil.
 :pin STR|nil
  Pin this package to commit hash STR. Setting this to nil will unpin the package
  if it was previously pinned.
Returns t if package is succesfully registered, and nil if it was disabled
elsewhere."
  (declare (indent defun))
  (when (and recipe (keywordp (car-safe recipe)))
    (wad--plist-put! plist :recipe `(quote ,recipe)))
  (when (equal type :built-in)
    (when (not ignore)
      (setq built-in `(locate-library ,(symbol-name name) nil
				      wad-initial-load-path)))
    (wad--plist-delete! plist :built-in)
    (wad--plist-put! plist :ignore built-in))
  `(list* ((name ',name)
	   (plist (cdr (assq name wad-packages))))
	  (let ((package-modules (or (plist-get plist :modules) '()))
		(module ',(wad--modules-from-path)))
	    (unless (member module package-modules)
	      (progn
		(wad--plist-put! plist :modules
				 (setf package-modules
				       (apennd package-modules (list module)))))))
	  (wad--loop-plist! ((prop val) (list ,@plist) plist)
	    (unless (null val)
	      (wad--plist-put! plist prop val)))
	  (setf (alist-get name wad-packages) plist)
	  (if (plist-get plist :disable)
	      (add-to-list 'wad-disabled-packages name)
	    (with-no-warnings (cons name plist)))))

;;;; Modules

(defvar wad--modules (make-hash-table :test 'equal)
  "A table of enabled modules being handled by wad.el.
It is highly discouraged to mutate `wad--modules' directly, and should only be
mutated via the associated `wad--modules-set' and `wad--modules-get' functions.
The key is a cons of (category . module) and the value is a plist of metadata
related to the module.")


(defvar wad--modules-init ()
  "List of modules who have been initialised via use of `wad--module-init'.
This list is used primarily by the `wad--module-init-p' function.")

(defvar wad--modules-init-packages ()
  "List of modules whose packages have been installed via the use of
`wad--module-init-packages'. This list is used primarily by the
`wad--module-init-packages-p' function.")

(defvar wad--modules-ensure-packages ()
  "List of modules whose packages have been ensured to be installed via the
use of `wad--module-ensure-packages'. This list is used primarily by the
`wad--module-ensure-package-p' function.")

(defvar wad--modules-config ()
  "List of configured modules, configured via the use of `wad--module--config'
function. This list is used primarily bt the `wad--module-config-p' function.")

(defvar wad--modules-current-module nil
  "The currently being processed module. Used throughout wad internally.")

(defun wad--modules-get (category module &optional prop)
  "Get a module or property if it has been already added via
`wad!' or `wad--modules-set'.
An optional PROP can be provided to fetch a property of the package definition if
it exists. Otherwise, the plist will be provided as is if it exists."
  (declare (pure t) (side-effect-free t))
  (when-let ((key (wad--module-key category module))
	     (plist (gethash key wad--modules)))
    (if prop (plist-get plist prop)
      plist)))

(defun wad--modules-set (category module &rest plist)
  "Adds a module by adding it to the `wad--modules'.
CATEGORY is a keyword, MODULE is a symbol, and PLIST is a plist that accepts the
following properties:
 - :path STRING - path toe category root directory"
  (let ((key (wad--module-key category module)))
    (puthash key plist wad--modules)))

(defun wad--modules-map (fn)
  "Apply FN to each module in `wad--modules'."
  (maphash (lambda (module plist)
	     (let ((category (car module))
		   (module (cdr module)))
	       (funcall fn category module plist)))
	   wad--modules))

(defun wad--modules-init ()
  (wad--modules-map
   (lambda (category module plist)
     (unless (wad--module-init-p category module)
       (run-hook-wrapped 'wad--hook-modules-before-init
			 #'wad--hook-try-run)
       (wad--module-init category module plist)
       (run-hook-wrapped 'wad--hook-modules-after-init
			 #'wad--hook-try-run)))))

(defun wad--modules-init-packages ()
  (wad--modules-map
   (lambda (category module plist)
     (unless (wad--module-init-packages-p category module)
       (run-hook-wrapped 'wad--hook-modules-before-init-packages
			 #'wad--hook-try-run)
       (wad--module-init-packages category module plist)
       (run-hook-wrapped 'wad--hook-modules-after-init-packages
			 #'wad--hook-try-run)))))

(defun wad--modules-ensure-packages ()
  (wad--modules-map
   (lambda (category module plist)
     (unless (wad--module-ensure-packages-p category module)
       (run-hook-wrapped 'wad--hook-modules-before-ensure-packages
			 #'wad--hook-try-run)
       (wad--module-ensure-packages category module plist)
       (run-hook-wrapped 'wad--hook-modules-after-ensure-packages
			 #'wad--hook-try-run)))))

(defun wad--modules-config ()
  (wad--modules-map
   (lambda (category module plist)
     (unless (wad--module-config-p category module)
       (run-hook-wrapped 'wad--hook-modules-before-config
			 #'wad--hook-try-run)
       (wad--module-config category module plist)
       (run-hook-wrapped 'wad--hook-modules-after-config
			 #'wad--hook-try-run)))))

(defvar wad--module-init-file "init"
  "Basename of init file for a module.")

(defvar wad--module-packages-file "packages"
  "Basename of packages file for a module.")

(defvar wad--module-config-file "config"
  "Basename of config file for a module.")

(defun wad--module-enabled-p (category module)
  "Returns t if CATEGORY MODULE is enabled."
  (declare (pure t) (side-effect-free t))
  (when-let ((key (wad--module-key category module))
	     (plist (gethash key wad--modules)))
    (not (null plist))))

(defun wad--module-init (category module &rest plist)
  "Loads `wad--module-init-file' for given module with CATEGORY and MODULE."
  (when (funcall (wad--module-file-loader wad--module-init-file) category module plist)
    (push (wad--module-key category module) wad--modules-init)))

(defun wad--module-init-p (category module)
  "Returns t if CATEGORY MODULE has been initialised via `wad--module-init-file'
in the associated module path."
  (if (member (wad--module-key category module) wad--modules-init) t))

(defun wad--module-init-packages (category module &rest plist)
  "Loads `wad--packages-file'"
  (when (funcall (wad--module-file-loader wad--module-packages-file) category module plist)
    (push (wad--module-key category module) wad--modules-init-packages)))

(defun wad--module-init-packages-p (category module)
  "Returns t if CATEGORY MODULE has been initialised via `wad--module-packages-file'
in the associated module path."
  (if (member (wad--module-key category module) wad--modules-init-packages) t))

(defun wad--module-ensure-packages (category module &rest plist)
  "Ensure all packages for a given module within CATEGORY and MODULE are available."
  (when (wad--module-packages-map #'wad--packages-ensure (wad--module-key category module))
    (push (wad--module-key category module) wad--modules-ensure-packages)))

(defun wad--module-ensure-packages-p (category module)
  "Returns t if CATEGORY MODULE have had their packages installed via `wad--module-packages-file'
in the associated module path."
  (if (member (wad--module-key category module) wad--modules-ensure-packages) t))

(defun wad--module-config (category module &rest plist)
  "Configure packages and module for a given module within CATEGORY and MODULE."
  (when (funcall (wad--module-file-loader wad--module-config-file) category module plist)
    (push (wad--module-key category module) wad--modules-config)))

(defun wad--module-config-p (category module)
  "Returns t if CATEGORY MODULE have had their packages and module configured via
`wad--module-config-file' in the associated module path."
  (if (member (wad--module-key category module) wad--modules-config) t))

(defun wad--module-key (category module)
  "Returns a cons cell of (CATEGORY . MODULE) where CATEGORY is a KEYWORD, and
MODULE is a symbole."
  (when (and (keywordp category) (symbolp module))
    (cons category module)))

(defun wad--module-path-from-key (category &optional module file)
  "Searches `wad--modules-directories' to find the path to a module.
CATEGORY is a keyword and MODULE is a symbol. Optionally, a FILE can be
provided that will be appended to the resulting path. If no path exists,
it returns nil, otherwise an absolute path."
  (when (keywordp category)
    (setq category (wad--keyword-to-string category)))
  (when (and module (symbol-name module))
    (setq module (symbol-name module)))
  (cl-loop with filename-handler-alist = nil
	   for default-directory in wad--modules-directories
	   for path = (concat category "/" module "/" file)
	   if (file-exists-p path)
	   return (file-truename path)))

(defun wad--module-key-from-path (&optional path enabled-only)
  "Returns a cons cell (CATEGORY . MODULE) via `wad--module-key' derived
from a file PATH. If ENABLED-ONLY, return nil if the containing module isn't
enabled."
  (if (null path)
      (if wad--modules-current-module
	  (if enabled-only
	      (and (wad-module-p (caar wad--modules-current-module)
				 (cdar wad--modules-current-module))
		   (car wad--modules-current-module))
	    (car wad--modules-current-module))
	(ignore-errors
	  (wad--module-key-from-path (wad--file-name))))
    (let* ((file-name-handler-alist nil)
	   (path (file-truename (or path (wad--file-name)))))
      (save-match-data
	(cond ((string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
               (when-let* ((category (wad--string-to-keyword (match-string 1 path)))
                           (module (intern (match-string 2 path))))
		 (and (or (null enabled-only)
                          (wad-module-p category module))
                      (wad--module-key category module)))))))))

(defun wad--module-file-loader (file)
  "Returns a closure that loads FILE from a given module.
The closure takes three arguments - the CATEGORY which is a keyword, a MODULE name
which is a symbol, and the matching module PLIST."
  (declare (pure t) (side-effect-free t))
  (lambda (category module plist)
    (let ((wad--modules-current-module (list (wad--module-key category module)
					     (wad--modules-get category module))))
      (wad--load file (plist-get plist :path)))))

(defun wad--module-packages-map (fn module)
  "Apply FN to each package declared in MODULE."
  (cl-loop for package in wad-packages
	   for name = (car package)
	   for modules = (wad--packages-get name :modules)
	   when (member module modules)
	   do (funcall fn name (cdr package))))

;;;; Packages

;; wad.el modules have the option to provide an associated `wad-package-file'
;; file. Containing one or more `wad-package!' declaration, which in turn
;; populate the list of packages that wad.el will ensure available when a
;; module is enabled. A `wad-package!' does *not* install a package, but simply
;; defines *how* to install it.

(defvar wad-packages ()
  "A list of enabled packages defined within enabled modules via wad.el.
It is highly discourages to mutate `wad-packages' directly, and should only
be mutated via `wad-packages-et' and accessed with `wad-packages-get'
functions.
Each element in `wad-packages' is a sublist, whose CAR is the package name as
a symbol, and whose CDR is the plist, initially supplied via the `wad-package!'
declaration.")

(defvar wad-disabled-packages ()
  "A list of disabled packages that should be ignored during initialisation.
Packages can be disabled using the `:disabled' keyword as part of the `wad-package!'
declaration.")

(defun wad--packages-add (package plist)
  "Add package to `wad-package'. Will replace any existing package."
  (setf (alist-get package wad-packages)
	(if (listp plist) plist (list plist))))

(defun wad--packages-get (package &optional prop nil-value)
  "Returns package plist matching PACKAGE. Optionally, if a PROPERTY has
been provided, and is a member of the package plist, it will be returned.
Returns nil if the package does not exist, is disabled, or the property does not
exist on the package plist, if provided."
  (let ((plist (cdr (assq package wad-packages))))
    (if prop
	(if (plist-member plist prop)
	    (plist-get plist prop)
	  nil-value)
      plist)))

(defun wad--packages-set (package prop value)
  "Sets property of package plist in `wad-packages'."
  (wad--plist-put! (wad--packages-get package) prop value))

(defun wad--packages-pinned ()
  "Return an alist mapping package names to pinned commits."
  (let (alist)
    (dolist (package wad-packages alist)
      (cl-destructuring-bind
	  (name
	   &key disable ignore pin unpin
	   &allow-other-keys)
	  package
	(when (and (not ignore)
		   (not disable)
		   (or pin unpin))
	  (setf (alist-get (format "%s" name) alist
			   nil 'remove #'equal)
		(unless unpin pin)))))))

(defvar wad--packages-ensure-handlers ()
  "A list of package handlers to use for ensuring a package is installed.
Each element in `wad--packages-ensure-handlers' is a sublist of cons cells,
whose CAR is the package name as a symbol, and whose CDR is the handler
itself, which is supplied using the `wad--packages-ensure-handler!' function.
A package handler is called by `wad--packages-ensure' with the associated
package name, plist, and recipe. It is the responsibility of the package
handler to correctly install and ensure the package is available to load.")

(defun wad--packages-ensure-handler (package plist recipe))

(defun wad--packages-ensure (package &optional plist)
  "Ensure that PACKAGE is installed. By default `wad' makes no decision
about how a package should be installed, and delegates to registered
handler(s)."
  (if-let ((plist (or plist (wad--packages-get package))))
      (let ((recipe (plist-get plist :recipe)))
	(if-let ((fn (alist-get package wad--packages-ensure-handlers)))
	    (funcall fn package plist recipe)
	  (wad--packages-ensure-handler package plist recipe)))))

(defvar wad--packages-load-handlers ()
  "A list of package handlers to use when loading a given package.
Package handlers can be used to override any existing package handler
when loading a package.
Each element in `wad--packages-load-handlers' is a sublist, whose CAR
is the package name as a symbol, and whose CDR is the handler itself, which
is supplied using the `wad--packages-load-handler!' function.
A package handler is called by `wad--packages-load' with the associated
package name, plist, and recipe. It is the responsibility of the package
handler to correctly load the package.")

(defun wad--packages-load-handler (package plist recipe))

(defun wad--packages-load (package &optional plist)
  "Ensure PACKAGE is loaded and ready for use. By default `wad' makes
no decision on how a package should be loaded, and delegates to registered
handler(s)."
  (if-let ((plist (or plist (wad--packages-get package))))
      (let ((recipe (plist-get plist :recipe)))
	(if-let ((fn (alist-get package wad--packages-load-handlers)))
	    (funcall fn package plist recipe))
	(wad--packages-load-handler package plist recipe))))

(defun wad--packages-reset ()
  "Reset `wad-packages' to initial state."
  (setq wad-packages ()))

;;;; Hooks

(define-error 'wad--hook-error
  "Error running wad.el hook"
  'wad--error)

(defun wad--hook-try-run (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (wad--log "Running hook: %s" hook)
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'wad--hook-error (list hook e))))
  nil)

;;;; Library helpers

(defun wad--directory-name ()
  "Returns the directory of the emacs lisp file this is called from."
  (when-let (path (wad--file-name))
    (directory-file-name (file-name-directory path))))

(defun wad--file-name ()
  "Returns path of the emacs lisp this macro is called from."
  (cond
   ((bound-and-true-p byte-compile-current-file))
   (load-file-name)
   ((stringp (car-safe current-load-list)) (car curent-load-list))
   (buffer-file-name)
   ((error "wad--file-name failed to retrieve file path"))))

(defun wad--path (path &optional base)
  "Return a path relative to the base path, where PATH is a string and BASE
is a string for the initial base path."
  (let ((base (or base (wad--directory-name)))
	default-directory)
    (expand-file-name path base)))

(defmacro wad--load (filename &optional path noerror)
  "Load a file relative to the current executing file."
  (let* ((path (or path (wad--directory-name)))
	 (file (if path `(expand-file-name ,filename ,path) filename)))
    `(condition-case-unless-debug e
	 (let (filename-handler-alist)
	   (load ,file ,noerror 'nomessage))
       (error "wad-load failed to load %s" file))))

(defun wad--process (command &rest args)
  "Execute COMMANd with ARGS synchronously.
Delegates to `wad--process-sync' when `wad--debug-p' is non-nil value,
and `wad--process-async' when it is nil."
  (apply (if wad--debug-p #'wad--process-sync #'wad--process-async)
	 command args))

(defun wad--process-sync (command &rest args)
  "Execute COMMAND with ARGS synchronously.
Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned
error code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args)) -1)
	  (string-trim (buffer-string)))))

(defun wad--process-async (command &rest args)
  "Execute COMMAND with ARGS asynchronously.
Unlike `wad--process-sync', this pipes output to `standard-output' on
the fly to simulate `exec' in the shell, so batch scripts can run external
programs synchronously without sacrificing their output."
  (with-temp-buffer
    (cons (let ((process
		 (make-process :name "wad-process"
			       :buffer (current-buffer)
			       :command (cons command (remq nil args))
			       :connection-type 'pipe))
		done-p)
	    (set-process-filter
	     process (lambda (_ output)
		       (princ output (current-buffer))
		       (princ output)))
	    (set-process-sentinel
	     process (lambda (process _)
		       (when (memq (process-status process) '(exit stop))
			 (setq done-p t))))
	    (while (not done-p)
	      (sit-for 0.1))
	    (process-exit-status process))
	  (string-trim (buffer-string)))))

(defmacro wad--log (format-string &rest args)
  "Log to *Messages* if `wad--debug-p' is non-nil."
  `(when wad--debug-p
     (let ((inhibit-message (active-minibuffer-window)))
       (message ,format-string ,@args))))

(defun wad--string-to-keyword (string)
  "Converts STRING to keyword."
  (declare (pure t) (side-effect-free t))
  (cl-check-type string string)
  (intern (concat ":" string)))

(defun wad--keyword-to-string (keyword)
  "Returns string of KEYWORD without leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun wad--ensure-list (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defmacro wad--plist-put! (plist &rest pairs)
  "Set each property value pair in PAIRS to PLIST in-place."
  `(cl-loop for (prop value)
	    on (list ,@pairs) by #'cddr
	    do ,(if (symbolp plist)
		    `(setq ,plist (plist-put ,plist prop value))
		  `(plist-put ,plist prop value))))

(defun wad--plist-delete (plist &rest props)
  "Delete PROPS from a copy of PLIST."
  (let (p)
    (while plist
      (if (not (memq (car plist) props))
	  (wad--plist-put! p (car plist) (nth 1 plist)))
      (setq plist (cddr plist)))
    p))

(defmacro wad--plist-delete! (plist prop)
  "Delete PROP from PLIST in-place."
  `(setq ,plist (wad--plist-delete ,plist ,prop)))

(cl-defmacro wad--loop-plist! ((arglist plist &optional retval) &body body)
  "Loop over a PLIST (property value) pairs and return RETVAL.
Evaluate BODY with either ARGLIST bound to (cons PROPERTY VALUE) or,
if ARGLIST is a list, the pair is destructured into (CAR . CDR)."
  (declare (indent 1))
  (let ((plist-var (make-symbol "plist")))
    `(let ((,plist-var (copy-sequence ,plist)))
       (while ,plist-var
	 (let ,(if (listp arglist)
		   `((,(pop arglist) (pop ,plist-var))
		     (,(pop arglist) (pop ,plist-var)))
		 `((,arglist (cons (pop ,plist-var)
				   (pop ,plist-var)))))
	   ,@body))
       ,retval)))

(defun wad--rpartial (fn &rest args)
  "Return a function that is a partial application of FN to right hand ARGS.
Args is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are
fixed at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun wad--http (host path)
  "Return url for HOST and PATH, defaulting to https if possible."
  (concat "https" "://" host "/" path))

(defun wad--github-url (path)
  "Return github host prefixed path."
  (wad--http "github.com" path))

(provide 'wad)

;;; wad.el ends here
