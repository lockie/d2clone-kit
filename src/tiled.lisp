(in-package :d2clone-kit)


(deftype rgba-color () '(simple-array unsigned-byte (4)))

(defstruct tiled-tileset
  (name nil :type string :read-only t)
  (first-id nil :type fixnum :read-only t)
  (tile-width nil :type fixnum :read-only t)
  (tile-height nil :type fixnum :read-only t)
  (tile-count nil :type fixnum :read-only t)
  (columns nil :type fixnum :read-only t)
  (spacing nil :type (or null fixnum) :read-only t)
  (margin nil :type (or null fixnum) :read-only t)
  (image-source nil :type string :read-only t)
  (tiles-properties nil :type (vector (or hash-table null)) :read-only t))

(defstruct tiled-layer
  (id nil :type fixnum :read-only t)
  (order nil :type fixnum :read-only t)
  (name nil :type string :read-only t)
  (width nil :type fixnum :read-only t)
  (height nil :type fixnum :read-only t)
  (properties nil :type hash-table :read-only t)
  (data nil :type (simple-array fixnum) :read-only t))

(defstruct tiled-object
  (id nil :type fixnum :read-only t)
  (name nil :type (or null string) :read-only t)
  (x nil :type fixnum :read-only t)
  (y nil :type fixnum :read-only t)
  (width nil :type (or null fixnum) :read-only t)
  (height nil :type (or null fixnum) :read-only t)
  (properties nil :type hash-table :read-only t)
  (text nil :type (or null string) :read-only t)
  ;; TODO special types instead of plain keywords?
  (type nil :type keyword :read-only t))

(defstruct tiled-map
  (format-version nil :type string :read-only t)
  (tiled-version nil :type string :read-only t)
  ;; TODO : change symbol to keyword where appropriate
  (orientation nil :type symbol :read-only t)
  (render-order nil :type symbol :read-only t)
  (width nil :type fixnum :read-only t)
  (height nil :type fixnum :read-only t)
  (tile-width nil :type fixnum :read-only t)
  (tile-height nil :type fixnum :read-only t)
  (stagger-axis nil :type (or null symbol) :read-only t)
  (stagger-index nil :type (or null symbol) :read-only t)
  (background-color nil :type (or null rgba-color) :read-only t)
  (tilesets nil :type (vector tiled-tileset) :read-only t)
  (layers nil :type (vector tiled-layer) :read-only t)
  (objects nil :type (vector tiled-object) :read-only t))
;; TODO
; note: unable to
;   avoid runtime dispatch on array element type
; due to type uncertainty:
;   The first argument is a (VECTOR TILED-LAYER), not a SIMPLE-ARRAY.

(defun load-tiled-map (stream)
  (labels
      ((symbolize (val) (if val (make-keyword (string-upcase val)) nil))
       (xmlrep-integer-attrib-value (name tag)
         (if-let (val (xmlrep-attrib-value name tag nil))
                 (parse-integer val :junk-allowed t)
                 nil))
       (parse-property-value (type val)
         (declare (type string val))
         (case type
           ((:bool) (string= val "true"))
           ((:color)
            (if (length= 7 val)
                (vector
                 (parse-integer val :start 1 :end 3 :radix 16)
                 (parse-integer val :start 3 :end 5 :radix 16)
                 (parse-integer val :start 5 :end 7 :radix 16)
                 255)
                (vector
                 (parse-integer val :start 3 :end 5 :radix 16)
                 (parse-integer val :start 5 :end 7 :radix 16)
                 (parse-integer val :start 7 :end 9 :radix 16)
                 (parse-integer val :start 1 :end 3 :radix 16))))
           ((:file) (pathname val))
           ((:float) (parse-float val))
           ((:int) (parse-integer val))
           (t val)))
       (tag-properties (tag)
         (let ((properties (make-hash-table :test #'eq))
               (properties-tag
                 (xmlrep-find-child-tag "properties" tag nil)))
           (when properties-tag
             (dolist (property (xmlrep-find-child-tags "property"
                                                       properties-tag))
               (setf
                (gethash
                 (symbolize (xmlrep-attrib-value "name" property nil))
                 properties)
                (if-let (value (xmlrep-attrib-value "value" property nil))
                  (parse-property-value
                   (symbolize (xmlrep-attrib-value "type" property nil))
                   value)
                  (xmlrep-string-child property)))))
           properties))
       (parse-tiles-properties (tag)
         (let* ((tileset-tile-count (xmlrep-integer-attrib-value "tilecount"
                                                                 tag))
                (tiles (xmlrep-find-child-tags "tile" tag))
                (result (make-array tileset-tile-count :initial-element nil)))
           (dolist (tile tiles)
             (when-let ((id (xmlrep-integer-attrib-value "id" tile))
                        (properties (tag-properties tile)))
               (setf (aref result id) properties)))
           result))
       (parse-tileset (tag)
         (make-tiled-tileset
          :name (xmlrep-attrib-value "name" tag nil)
          :first-id (xmlrep-integer-attrib-value "firstgid" tag)
          :tile-width (xmlrep-integer-attrib-value "tilewidth" tag)
          :tile-height (xmlrep-integer-attrib-value "tileheight" tag)
          :tile-count (xmlrep-integer-attrib-value "tilecount" tag)
          :columns (xmlrep-integer-attrib-value "columns" tag)
          :spacing (xmlrep-integer-attrib-value "spacing" tag)
          :margin (xmlrep-integer-attrib-value "margin" tag)
          :image-source (if-let (image (xmlrep-find-child-tag "image" tag nil))
                          (xmlrep-attrib-value "source" image nil)
                          "")
          :tiles-properties (parse-tiles-properties tag)))
       (parse-layer (tag order)
         (labels
             ((csv-layer-parser (data)
                (let* ((rows (cl-csv:read-csv
                              (make-string-input-stream data)
                              :unquoted-empty-string-is-nil t
                              :map-fn
                              #'(lambda (row)
                                  (mapcar #'parse-integer (remove nil row)))))
                       (row-count (length rows))
                       (col-count (length (first rows))))
                  (make-array (list row-count col-count)
                              :element-type 'fixnum
                              :initial-contents rows)))
              (base64-layer-parser (data width height &key compression)
                (let* ((decompressor (if (string= compression "zlib")
                                         #'(lambda (in)
                                             (chipz:decompress
                                              nil 'chipz:zlib
                                              (make-array (length in)
                                               :element-type '(unsigned-byte 8)
                                               :initial-contents in)
                                              :buffer-size (* 4 width height)))
                                         #'identity))
                       (result
                         (make-array (list height width) :element-type 'fixnum))
                       (raw-data (funcall decompressor
                                          (qbase64:decode-string data)))
                       (stream (make-instance 'virtual-binary-stream
                                              :buffer (make-array
                                                       (length raw-data)
                                                       :element-type
                                                       '(unsigned-byte 8)
                                                       :initial-contents
                                                       raw-data))))
                  (dotimes (i (* width height))
                    (setf (row-major-aref result i)
                          (read-binary 'dword stream)))
                  result)))
           (let ((data-tag (xmlrep-find-child-tag "data" tag nil)))
             (unless data-tag
               (error "Invalid layer in tmx file."))
             (let ((width (xmlrep-integer-attrib-value "width" tag))
                   (height (xmlrep-integer-attrib-value "height" tag)))
               (make-tiled-layer
                :id (xmlrep-integer-attrib-value "id" tag)
                :order order
                :name (xmlrep-attrib-value "name" tag nil)
                :width width
                :height height
                :properties (tag-properties tag)
                :data
                (let ((data (xmlrep-string-child data-tag nil)))
                  (case (symbolize (xmlrep-attrib-value "encoding" data-tag))
                    (:base64 (base64-layer-parser
                             data width height
                             :compression (xmlrep-attrib-value "compression"
                                                               data-tag "")))
                    (:csv (csv-layer-parser data))
                    (t (make-array '(0 0) :element-type 'fixnum)))))))))
       (parse-object (tag)
         (make-tiled-object
          :id (xmlrep-integer-attrib-value "id" tag)
          :name (xmlrep-attrib-value "name" tag nil)
          :x (xmlrep-integer-attrib-value "x" tag)
          :y (xmlrep-integer-attrib-value "y" tag)
          :width (xmlrep-integer-attrib-value "width" tag)
          :height (xmlrep-integer-attrib-value "height" tag)
          :properties (tag-properties tag)
          :text (if-let (text-tag (xmlrep-find-child-tag "text" tag nil))
                  (xmlrep-string-child text-tag nil)
                  nil)
          :type (if-let (object-type-child
                         (remove-if
                          #'(lambda (child-name)
                              (or (string= child-name "properties")
                                  (string= child-name "text")))
                          (mapcar #'xmlrep-tag (xmlrep-children tag))))
                  (symbolize (first object-type-child))
                  :rectangle))))
    (let ((map-tag (parse stream)))
      (unless map-tag
        (error "Invalid tmx file."))
      (make-tiled-map
       :format-version (xmlrep-attrib-value "version" map-tag nil)
       :tiled-version (xmlrep-attrib-value "tiledversion" map-tag nil)
       :orientation (symbolize (xmlrep-attrib-value "orientation" map-tag nil))
       :render-order (symbolize (xmlrep-attrib-value "renderorder" map-tag nil))
       :width (xmlrep-integer-attrib-value "width" map-tag)
       :height (xmlrep-integer-attrib-value "height" map-tag)
       :tile-width (xmlrep-integer-attrib-value "tilewidth" map-tag)
       :tile-height (xmlrep-integer-attrib-value "tileheight" map-tag)
       :stagger-axis (symbolize (xmlrep-attrib-value "staggeraxis" map-tag nil))
       :stagger-index (symbolize (xmlrep-attrib-value "staggerindex"
                                                      map-tag nil))
       :background-color (if-let (tag (xmlrep-attrib-value "backgroundcolor"
                                                           map-tag nil))
                           (parse-property-value :color tag)
                           nil)
       :tilesets (map 'vector #'parse-tileset (xmlrep-find-child-tags "tileset"
                                                                      map-tag))
       :layers (let ((order 0))
                 (map 'vector
                      #'(lambda (tag) (parse-layer tag
                                                   (incf (the fixnum order))))
                      (xmlrep-find-child-tags "layer" map-tag)))
       :objects (if-let (object-group (xmlrep-find-child-tag "objectgroup"
                                                             map-tag nil))
                  (map 'vector #'parse-object (xmlrep-find-child-tags
                                               "object"
                                               object-group))
                  #())))))
