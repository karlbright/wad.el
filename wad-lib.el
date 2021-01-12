;;; wad-lib.el -*- lexical-binding: t; -*-

(define-error 'wad/error "Error")

(defconst wad/is-mac
  (eq system-type 'darwin))

(defconst wad/is-linux
  (eq system-type 'gnu/linux))

(defconst wad/is-windows
  (memq system-type '(cygwin windows-nt ms-dos)))

(defconst wad/is-bsd
  (or wad/is-mac (eq system-type 'berkeley-unix)))

(defmacro wad/log (format-string &rest args)
  "Log to *Messages* if `wad/debug-p' is on."
  `(when wad/debug-p
    (let ((inhibit-message (active-minibuffer-window)))
      (message ,format-string ,@args))))

(defun wad/path (&rest segments)
  "Constructs a file path from SEGMENTS. Ignoring nil."
  (if segments
      (let ((segments (delq nil segments)) dir)
	(while segments
	  (setq dir (expand-file-name (car segments) dir)
		segments (cdr segments)))
	dir)
    (wad/current-file!)))

(defun wad/relative-path (&rest segments)
  (apply 'wad/path (wad/current-dir!) segments))

(defun wad/current-dir! ()
  "Returns the directory of the emacsl isp file this macro
has been called from."
  (when-let (path (wad/current-file!))
    (directory-file-name (file-name-directory path))))

(defun wad/current-file! ()
  "Returns the emacs lisp file this is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
	 (load-file-name)
	 ((stringp (car-safe current-load-list))
	  (car current-load-list))
	 (buffer-file-name)
	 ((error "Cannot get this file-path"))))

(defun wad/current-directory! ()
  "Returns the directory of the emacs lisp file this is called from."
  (when-let (path (wad/current-file!))
    (directory-file-name (file-name-directory path))))

(defmacro wad/load! (filename &optional path noerror)
  "Load a file relative to the current executing file (`load-file-name')."
  (let* ((path (or path
		   (wad/current-directory!)
		   (error "Could not detect path to look for %s in" filename)))
	 (file (if path
		   `(expand-file-name ,filename ,path)
		 filename)))
    `(condition-case-unless-debug e
	 (let (file-name-handler-alist)
	   (load ,file ,noerror 'nomessage))
       (error "Could not load file"))))

(cl-defmacro wad/doplist! ((arglist plist &optional result) &rest body)
  "Loop over PLIST (property value) pair, evaluating BODY for each
pair. Then evaluating and returning RESULT."
  (declare (indent 1))
  (let ((seq (make-symbol "seq")))
    `(let ((,seq (copy-sequence ,plist)))
       (while ,seq
	 (let ((,(pop arglist) (pop ,seq))
	       (,(pop arglist) (pop ,seq)))
	   ,@body))
       result)))

(defmacro wad/plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
	    on (list ,@rest) by #'cddr
	    do ,(if (symbolp plist)
		    `(setq ,plist (plist-put ,plist prop value))
		  `(plist--put ,plist prop value))))

(defun wad/keyword-to-string (keyword)
  "Returns the string of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun wad/string-to-keyword (str)
  "Converts STR into a keywoord (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun wad/string-to-symbol (str)
  "Converts STR into a symbol (`symbolp')."
  (intern str))

(defun wad/github-url (repository)
  "Returns string of github url with provided PATH."
  (concat "https://github.com/" repository))

(defun wad/call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil (remq nil args))
	      -1)
	  (string-trim (buffer-string)))))

(defun wad/exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Unlike `wad/call-process', this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell, so batch scripts could run external programs
synchronously without sacrificing their output."
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "wad"
                               :buffer (current-buffer)
                               :command (cons command (remq nil args))
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (_process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defun wad/unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(provide 'wad-lib)
