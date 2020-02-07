#!/usr/bin/env -S sbcl --script

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :deploy)

(push (truename ".") asdf:*central-registry*)
(asdf:make :d2clone-kit)
