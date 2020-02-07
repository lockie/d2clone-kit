#!/usr/bin/env -S sbcl --script

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :staple)
(ql:quickload :staple-markdown)

(staple:generate :d2clone-kit :if-exists :supersede :images '(#P"d2clone.png"))
