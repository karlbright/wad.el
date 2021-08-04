;;; modules/plugins/straight/packages.el -*- lexical-binding: t; no-byte-compile: t; -*-

(wad--package! straight
  :recipe (:repo "raxod502/straight.el" :branch "develop"))

(defun wad--plugin-straight-ensure (package plist recipe &rest _)
  "Ensure packages are available for use using straight.el."
  (wad--log "wad--plugin-straight-ensure %s %s %s" package plist recipe)
  (let* ((name package)
	 (repo (symbol-name name)))
    (if (not (null recipe))
	(straight-override-recipe (cons package recipe)))
    (straight-use-package package nil 'no-build)
    (or (member (directory-file-name
		 (straight--build-dir (symbol-name name)))
		load-path)
	(add-to-list 'load-path
		     (directory-file-name (straight--repos-dir repo))))))

(defun wad--plugin-straight-load (package plist &optional recipe)
  "Ensure packages are available for use using straight.el."
  (wad--log "wad--plugin-straight-load %s %s %s" package plist recipe)
  (if (not (null recipe))
      (straight-override-recipe (cons package recipe)))
  (straight-use-package package nil nil))

(wad--packages-override-ensure-handler!
 'straight
 (lambda (package plist recipe)
   (let ((pin (plist-get plist :pin))
	 (repo-directory (wad--path "straight/repos/straight.el" straight-base-dir))
	 (repo-url (wad--github-url (plist-get recipe :repo)))
	 (branch (or straight-repository-branch (plist-get recipe :branch))))
     (unless (file-directory-p repo-directory)
       (message "installing straight.el...")
       (cond
	((eq straight-vc-git-default-clone-depth 'full)
	 (wad--process "git" "clone" "--origin" "origin" repo-url repo-directory))
	((null pin)
	 (wad--process "git" "clone" "--origin" "origin" repo-url repo-directory
		      "--depth" (number-to-string straight-vc-git-default-clone-depth)
		      "--branch" branch
		      "--single-branch"
		      "--no-tags"))
	((integerp straight-vc-git-default-clone-depth)
	 (make-directory-p repo-directory t)
	 (let ((default-directory repo-directory))
	   (wad--process "git" "init")
	   (wad--process "git" "checkout" "-b" branch)
	   (wad--process "git" "remote" "add" "origin" repo-url)
	   (wad--process "git" "fetch" "origin" pin
			"--depth" (number-to-string straight-vc-git-default-clone-depth)
			"--no-tags")
	   (wad--process "git" "checkout" "--detach" pin)))))
     (require 'straight (wad--path "straight.el" repo-directory))
     (wad--log "initializing recipes...")
     (with-temp-buffer
       (insert-file-contents (wad--path "bootstrap.el" repo-directory))
       (eval-region (search-forward "(require 'straight)")
		    (point-max))))
   (wad--packages-default-ensure-handler! #'wad--plugin-straight-ensure)
   (wad--packages-default-load-handler! #'wad--plugin-straight-load)))

