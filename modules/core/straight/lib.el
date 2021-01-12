;;; modules/core/straight/lib.el -*- lexical-binding: t; -*-

(defun wad/ensure-straight (name plist)
  (let* ((pin (plist-get plist :pin))
	 (call (if wad/debug-p #'wad/exec-process #'wad/call-process))
	 (url straight-repository-url)
	 (dir (concat straight-base-dir "straight/"))
	 (branch straight-repository-branch)
	 (depth straight-vc-git-default-clone-depth))
    (unless (file-directory-p dir)
      (cond ((eq depth 'full)
	     (funcall call "git" "clone" "--origin" "origin" url dir))
	    ((null pin)
	     (funcall call "git" "clone" "--origin" "origin" url dir
		      "--depth" (number-to-string depth)
		      "--branch" branch
		      "--single-branch" "--no-tags"))
	    ((integerp depth)

	     (make-directory dir t)
	     (let ((default-directory dir))
	       (funcall call "git init")
	       (funcall call "git" "checkout" "-b" branch)
	       (funcall call "git" "remote" "add" "origin" url)
	       (funcall call "git" "fetch" "origin" pin
			"--depth" (number-to-string depth)
			"--no-tags")))))
    (require 'straight (wad/path dir "straight.el"))
    (wad/log "Initializing recipes")
    (with-temp-buffer
      (insert-file-contents (wad/path dir "bootstrap.el"))
      (eval-region (search-forward "(require 'straight)")
		   (point-max)))))

(provide 'wad/straight-lib)
