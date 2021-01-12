;;; wad.el --- Opinionated emacs configuration inspired by Doom Emacs. -*- lexical-binding: t; -*-

(defconst wad/version "0.0.1")

;;; State

(defconst wad/directory
  (directory-file-name
   (file-name-directory
    (or load-file-name buffer-file-name)))
    "Directory containing the wad library files.
Used primarily as part of `load-path'.")

(defconst wad/debug-p
  t
;;  (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, wad will log more information.")

(defvar wad/current-module nil)

(add-to-list 'load-path wad/directory)

(require 'cl-lib)
(require 'wad-config)
(require 'wad-lib)
(require 'wad-security)
(require 'wad-hooks)
(require 'wad-packages)
(require 'wad-modules)
(require 'wad-bootstrap)

(provide 'wad)
