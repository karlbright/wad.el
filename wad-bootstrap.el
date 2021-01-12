;;; wad-bootstrap.el -*- lexical-binding: t; -*-

 (defun wad/init! (&optional no-install-p)
  "Initializes modules found in `wad/modules'."
  (wad/modules-map
   (lambda (category module plist)
     (wad/module-log category module "wad/init")
     (wad/initialize-module category module plist)
     (wad/initialize-module-packages category module plist)
     (unless no-install-p
       (wad/ensure-module-packages category module plist)))))

(defun wad/config! ()
  "Configures modules found in `wad/modules'."
  (wad/modules-map #'wad/configure-module))

(defmacro wad! (&rest modules)
  "Define modules, initialize, and configure them."
  `(progn
     (wad/modules! ,@modules)
     (wad/init!)
     (wad/config!)))

(provide 'wad-bootstrap)
