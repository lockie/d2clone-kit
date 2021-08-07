(in-package :d2clone-kit)

;; NOTE : column names are case insensitive

(defun load-castledb-tables (stream)
  (loop
    :with sheets := (getf
                     (let ((jonathan:*null-value* :null))
                       (jonathan:parse
                        (read-stream-content-into-string stream
                                                         :buffer-size
                                                         (stream-size stream))
                        :keyword-normalizer #'string-upcase
                        :normalize-all t))
                     :sheets)
    :with result := (make-hash :test #'eq :size (length (the list sheets)))
    :for sheet :in sheets
    :for columns := (getf sheet :columns)
    :for column-processors :=
       (loop :for column :in columns
             :for column-type := (parse-integer
                                   (getf column :typestr "")
                                   :junk-allowed t)
             :append (list (make-keyword
                            (string-upcase
                             (getf column :name)))
                           (ecase column-type
                             (1 #'(lambda (text)
                                    (with-input-from-string (s text)
                                      (read s)))))))
    :for lines := (getf sheet :lines)
    :do (setf (gethash
               (make-keyword
                (string-upcase
                 (substitute #\- #\_ (getf sheet :name))))
               result)
              (dolist (line lines lines)
                (doplist (column value line)
                         (setf (getf line column)
                               (funcall
                                (the function (getf column-processors column))
                                value)))))
    :finally (return result)))
