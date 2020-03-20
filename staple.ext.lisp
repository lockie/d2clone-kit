(asdf:load-system :staple-markdown)

(defclass my-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :d2clone-kit))))
  'my-page)

(defmethod staple:format-documentation ((docstring string) (page my-page))
  (flet ((replace-see (string start end mstart mend rstart rend)
           (declare (ignore start end))
           (let* ((match (subseq string (aref rstart 0) (aref rend 0)))
                  (identifier (plump:decode-entities match))
                  xref)
             (cond ((cl-ppcre:scan "^[-a-zA-Z]+://" identifier)
                    (format NIL "See <a href=\"~a\" class=\"exref\">~a</a><br>"
                            match match))
                   ((setf xref (staple:xref identifier))
                    (format NIL "See <a href=\"~a\" class=\"xref\">~a</a><br>"
                            (plump:encode-entities xref) match))
                   (T
                    (subseq string mstart mend))))))
    (let* ((docstring (plump:encode-entities docstring))
           (docstring (cl-ppcre:regex-replace-all "[sS]ee (.*)" docstring #'replace-see))
           (*package* (first (staple:packages page))))
      (staple:markup-code-snippets-ignoring-errors
       (staple:compile-source docstring :markdown)))))

(defmethod staple:documents ((system (eql (asdf:find-system :staple))))
  (list (asdf:system-relative-pathname system "README.md")))

(defmethod staple:definition-wanted-p ((definition definitions:method) (__ my-page))
  (let* ((designator (definitions:designator definition))
         (symbol (etypecase designator
                   (cons (cadr designator))
                   (symbol designator))))
    (and
     (string= "D2CLONE-KIT" (package-name (symbol-package symbol)))
     (some
      (lambda (method) (documentation method 't))
      (closer-mop:generic-function-methods (symbol-function symbol))))))
