;;; modules/plugins/straight/init.el -*- lexical-binding: t; -*-

;; Enabling `straight' for package management will assume that package.el should
;; be disabled by default. This is not a bad thing as there are a few reasons to
;; avoid package.el. ELPA sources can suffer downtime and often fail to build
;; packages when GNU tar is not available on something like Mac OS X. There are
;; a number of TLS issues that cause issues for stable releases, which break
;; TLS handshake with ELPA repos.

;;;; package.el

(setq package-enable-at-startup nil)

;;;; Configuration

;; Install straight into local directory
(setq straight-base-dir wad--directory-local)

;; Default to the develop branch of straight.el
(setq straight-repository-branch "develop")

;; Byte code is rarely compatible across different versions of Emacs. It is best
;; to build them against emacs versions, in seperate directories.
(setq straight-build-dir (format "build-%s" emacs-version))

;; If non-nil, straight will cache autoloads into a single file onto disk,
;; reducing the number of disk IO operations during startup.
;; TODO(karlbright): How should this be handled via wad.el?
(setq straight-cache-autoloads nil)

;; Do not check for local file modiciations when initialising straight.el.
;; This slows down startup, and it's best to avoid local file modifications.
(setq straight-check-for-modifications nil)

;; We've already disabled package.el so we want to ensure it's not part of
;; straight.el via package integration at all.
(setq straight-enable-package-integration nil)

;; Default clone depth to 1 to reduce size of our `wad--local-directory', some
;; packages can break when shallow cloned, but that can be dealt with on a
;; per-package basis. This is a sane default.
(setq straight-vc-git-default-clone-depth 1)

;; Disable the bulk added by autoload prefix declarations.
(setq autoload-compute-prefixes nil)

;; This should be handled by org module
(setq straight-fix-org nil)


