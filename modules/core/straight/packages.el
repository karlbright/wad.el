;;; modules/core/straight/packages.el -*- lexical-binding: t; -*-

(require 'wad/straight-lib (wad/relative-path "lib.el"))

(wad/package! straight :recipe #'wad/ensure-straight)

