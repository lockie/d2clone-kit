(in-package :d2clone-kit)

;; TODO : redo with plain sexps! http://people.csail.mit.edu/rivest/Sexp.txt

(declaim (type hash-table *data-tables*))
(global-vars:define-global-var *data-tables* (make-hash :test #'eq)
  "Global data tables read from CastleDB resource files.")

(declaim (ftype (function (list list list) hash-table) build-data-table))
(defun build-data-table (table index-columns value-columns)
  (loop :with result := (make-hash :test #'equal)
        :for row :in table
        :for indices := (mapcan
                         #'(lambda (column) (list column (getf row column)))
                         index-columns)
        for values := (if (length= 1 value-columns)
                          (getf row (first value-columns))
                          (mapcan
                           #'(lambda (column) (list column (getf row column)))
                           value-columns))
        :do (setf (gethash indices result) values)
        :finally (return result)))

(define-constant +table-indices+
    '(:impact-sounds ((:armor-class :weapon-class) (:sound))
      )
  :test #'equal
  :documentation "Specifies the columns to be included in indices for data
  tables. It is assumed that the columns list is alphabetically sorted.")

(declaim (ftype (function (hash-table list)) build-data-tables))
(defunl build-data-tables (data table-indices)
  "Builds data table indices for given DATA and TABLE-INDICES

See +TABLE-INDICES+"
  (loop :with indices := (append +table-indices+ table-indices)
        :with result := (make-hash :test #'eq)
        :for count :of-type fixnum :from 0
        :for designator :being :the :hash-key
        :using (hash-value table) :of data
        :for (index-columns value-columns) := (getf indices designator)
        :do (setf (gethash designator result)
                  (build-data-table table index-columns value-columns))
        :finally (log-info "~d data table(s) built" count)
        :finally (return result)))

(defunl load-data-tables (table-indices)
  "Loads all possible data from CastleDB files, considering table priorities.
Afterwards, builds all the appropriate indices and sets *DATA-TABLES*
accordingly.

See *DATA-TABLES*"
  (let ((all-tables (make-hash :test #'eq))
        (all-priorities (make-hash :test #'eq)))
    (enumerate-directory "tables"
      (when (uiop:string-suffix-p file ".cdb")
        (log-info "Loading data table ~a" file)
        (multiple-value-bind (tables priorities)
            (load-castledb-tables
             (make-instance 'character-stream
                            :path (namestring
                                   (make-pathname
                                    :defaults (pathname file)
                                    :directory `(:relative ,directory)))))
          (loop :for table-name :being :the :hash-key
                :using (hash-value rows) :of tables
                :for priority := (gethash table-name priorities)
                :do (multiple-value-bind (old-priority existing)
                        (gethash table-name all-priorities)
                      (when (or (not existing)
                                (< old-priority priority))
                        (setf
                         (gethash table-name all-tables) rows
                         (gethash table-name all-priorities) priority)))))))
    (setf *data-tables* (build-data-tables all-tables table-indices))))

(declaim
 #-d2c-debug (inline table-value-ref)
 (ftype
  (function (keyword list &key (:default t) (:column (or keyword null))) t)
  table-value-ref))
(defun table-value-ref (table-designator predicates
                        &key (default nil) (column nil))
  "References the data table designated by TABLE-DESIGNATOR by plist
PREDICATES, returning either (1) the column value, if it is just a single
column, (2) a plist of columns if there are multiple, or (3) DEFAULT, if no
row corresponding to PREDICATES is found. COLUMN, if non-NIL, specifies the
column to return in (2) case. This function assumes that the column names in
PREDICATES are alphabetically sorted.

See TABLE-VALUE-REF*"
  (if-let (table (gethash table-designator *data-tables*))
    (if-let (row (gethash predicates table))
      (if column
          (getf row column)
          row)
      default)
    default))

(declaim
 #-d2c-debug (inline table-value-ref*)
 (ftype (function (keyword list &key (:default t) (:column keyword)) t)
        table-value-ref*))
(defun table-value-ref* (table-designator predicates
                         &key (default nil) (column nil))
  "References the data table designated by TABLE-DESIGNATOR by plist
PREDICATES, returning either (1) the column value, if it is just a single
column, (2) a plist of columns if there are multiple, or (3) DEFAULT, if no
row corresponding to PREDICATES is found. COLUMN, if non-NIL, specifies the
column to return in (2) case. This function makes sure that the column names
in PREDICATES are alphabetically sorted and therefore works slightly slower.

See TABLE-VALUE-REF"
  (table-value-ref table-designator
                   (sort predicates
                         #'string<
                         :key #'car)
                   :default default
                   :column column))
