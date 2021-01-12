;;; modules/core/straight/init.el -*- lexical-binding: t; -*-

(let ((recipe (wad/package-get 'straight :recipe)))
  (setq straight-base-dir wad/vendor-dir
	straight-repository-url (wad/github-url "raxod502/straight.el")
	straight-repository-branch (or (plist-get recipe :branch) "develop")
	straight-build-dir (format "build-%s" emacs-version)
	straight-cache-autoloads nil
	straight-check-for-modifications nil
	straight-enable-package-integration nil
	straight-vc-git-default-clone-depth 1
	autoload-compute-prefixes nil
	straight-fix-org nil))

(with-eval-after-load 'straight
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))
