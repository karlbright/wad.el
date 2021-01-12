;;; modules/core/package/init.el -*- lexical-binding: t; -*-

;; We prefer `straight' over package.el. ELPA sources can suffer downtime and
;; often fail to build packages when GNU tar is unavailable. There are a
;; number of TLS issues that plague stable releases, which break TLS handshake
;; with ELPA repos. See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434
;;
;; Package.el also only allows us to fetch the latest version of packages
;; through ELPA. In an ecosystem that is constantly changing, this is more
;; frustrating than convenient.
;;
;; We disable package.el by default, assuming `modules/core/straight' has been
;; enabled.

(setq package-enable-at-startup nil
      package-user-dir (concat wad/vendor-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir))

;; Ensure that package.el does not modify init.el at all
(advice-add #'package--ensure-init-file :override #'ignore)

;; Refresh package.el the first time you call `package-install', so it can still
;; be used, like to test packages temporarily.
(wad/add-transient-hook! 'package-install (package-refresh-contents))
