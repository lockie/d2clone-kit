#!/usr/bin/env -S sbcl --script

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(push (truename ".") asdf:*central-registry*)
(ql:quickload '(:staple :staple-markdown :d2clone-kit))
(pushnew :docs *features*)

(staple:generate :d2clone-kit
                 :if-exists :supersede :images '(#P"d2clone.png")
                 :output-directory
                 (if (member "-p" (uiop:command-line-arguments) :test #'string=)
                     #P"public/"
                     #P"docs/"))
