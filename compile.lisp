;;; Helper script to compile the :d2clone-kit system within CI pipeline

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push (truename ".") asdf:*central-registry*)
(ql:quickload :d2clone-kit)
(quit)
