#!/usr/bin/env -S sbcl --script

(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(push (truename ".") asdf:*central-registry*)
(ql:quickload '(:lisp-critic :d2clone-kit))

(loop :for pathname :in (uiop:directory-files "src/*")
      :when (uiop:string-suffix-p (namestring pathname) ".lisp")
      :do (lisp-critic:critique-file pathname))
