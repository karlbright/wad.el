;;; modules/core/straight/config.el -*- lexical-binding: t; -*-

(defun wad/ensure-package-handler (name package)
  (when-let (recipe (plist-get package :recipe))
    (straight-override-package (cons name recipe)))
  (straight-use-package name))

(eval-after-load 'straight '(wad/ensure-modules-packages))


