;;; wad-config.el -*- lexical-binding: t; -*-

(defconst wad/core-modules-dir
  (file-chase-links
   (expand-file-name
    "modules/"
    (file-name-directory
     (or load-file-name buffer-file-name)))))

(defvar wad/modules-dirs (list wad/core-modules-dir)
  "A list of module root directories. Order determines priority.")

;; TODO(karlbright): Change name for this?

(defvar wad/vendor-dir
  (expand-file-name "vendor/" user-emacs-directory))

(defvar wad/etc-dir
  (expand-file-name "etc/" user-emacs-directory))

(defvar wad/module-init-file "init"
  "Basename of init files for modules.")

(defvar wad/module-config-file "config"
  "Basename of config files for modules.")

(defvar wad/module-packages-file "packages"
  "Basename of packages files for modules.")

(provide 'wad-config)
