;;; modules/plugins/use-package/config.el -*- lexical-binding: t; -*-

(autoload 'use-package "use-package-core" nil nil t)

;;; Add use-package functionality to wad.el

(defun wad--plugin-use-package-use (package plist args)
  (eval `(use-package ,package ,@args)))

(wad--packages-default-use-handler! #'wad--plugin-use-package-use)


