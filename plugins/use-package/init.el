;;; modules/plugins/use-package/init.el -*- lexical-binding: t; -*-

(autoload 'use-package-core "use-package-core" nil nil t)

(setq use-package-compute-statistics wad--debug-p
      use-package-verbose (if wad--debug-p 'debug nil)
      use-package-minimum-reported-time (if wad--debug-p 0 0.1)
      use-package-expand-minimally wad--interactive-p)

(setq use-package-ensure-function
      (lambda (name &rest _)
	(message "Ignoring ':ensure t' in '%s' config" name)))

(with-eval-after-load 'use-package-core
  ;; `use-package' adds syntax highlighting for the `use-package' macro, but
  ;; Emacs 26+ already highlights macros, so it's redundant.
  (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords))
