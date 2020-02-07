(asdf:load-system :staple-markdown)

(defclass my-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :d2clone-kit))))
  'my-page)

(defmethod staple:format-documentation ((docstring string) (page my-page))
  (let ((*package* (first (staple:packages page))))
    (staple:markup-code-snippets-ignoring-errors
     (staple:compile-source docstring :markdown))))
