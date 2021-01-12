;;; wad-hooks.el -*- lexical-binding: t; -*-

(defvar wad/transient-counter 0)

(defmacro wad/add-transient-hook! (hook-or-function &rest forms)
  "Attach a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then
never again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function, which will
be advised."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
	(fn (intern (format "wad/transient-%d-h" (cl-incf wad/transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
	 ,(format "Transient hook for %S" (wad/unquote hook-or-function))
	 ,@forms
	 (let ((sym ,hook-or-function))
	   (cond ((functionp sym) (advice-remove sym #',fn))
		 ((symbolp sym) (remove-hook sym #',fn))))
	 (unintern ',fn nil))
       (cond ((functionp sym)
	      (advice-add ,hook-or-function ,(if append :after :before) #',fn))
	     ((symbolp sym)
	      (put ',fn 'permanent-local-hook t)
	      (add-hook sym #',fn ,append))))))

(defun wad/try-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (wad/log "Running hook: %s" hook)
  (condition-case e
      (funcall hook)
    ((debug error)
     (signal 'wad/hook-error (list hook e))))
  nil)

(defun wad/run-hook-once-on (hook-var triggers)
  "Configure HOOK-VAR to be invoked exactly once after init whenever any of the
TRIGGERS are invoked. Once HOOK-VAR gets triggered, it resets to nil.

HOOK-VAR is quoted hook.

TRIGGERS is a list of quoted hooks and/or sharp-quoted functions."
  (let ((fn (intern (format "%s-hook" hook-var))))
    (fset
     fn (lambda (&rest _)
	  (when after-init-time
	    (run-hook-wrapped hook-var #'wad/try-run-hook)
	    (set hook-var nil))))
    (put hook-var 'permanent-local t)
    (dolist (on triggers)
      (if (functionp on)
	  (advice-add on :before fn)
	(add-hook on fn)))))

(define-error 'wad/hook-error "Error in hook" 'wad/error)

(defvar wad/first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar wad/first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar wad/first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(wad/run-hook-once-on 'wad/first-file-hook '(after-find-file dired-initial-position-hook))
(wad/run-hook-once-on 'wad/first-buffer-hook '(after-find-file wad/switch-buffer-hook))
(wad/run-hook-once-on 'wad/first-input-hook '(pre-command-hook))

(provide 'wad-hooks)
