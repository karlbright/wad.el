;;; modules/core/gcmh/config.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'wad/first-input-hook #'gcmh-mode)
