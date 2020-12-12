(in-package :d2clone-kit)


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
        :for count fixnum :from 0
        :for designator :being :the :hash-key
        :using (hash-value table) :of data
        :for (index-columns value-columns) := (getf indices designator)
        :do (setf (gethash designator result)
                  (build-data-table table index-columns value-columns))
        :finally (log-info "~d data table(s) built" count)
        :finally (return result)))

(declaim
 (inline table-value-ref)
 (ftype (function (keyword list &key (:default t) (:column keyword)) t)
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
 (inline table-value-ref*)
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
