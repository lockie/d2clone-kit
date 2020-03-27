(in-package :d2clone-kit)


(defmethod read-binary ((type (eql 'ase-string)) stream)
  (let* ((size (read-binary 'word stream))
         (string (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence string stream)
    (babel:octets-to-string string)))

(define-binary-struct ase-binary-header
  (file-size :type 'dword)
  (magic :type 'word)
  (frames :type 'word)
  (width :type 'word)
  (height :type 'word)
  (color-depth :type 'word)
  (flags :type 'dword)
  (speed :type 'word)
  (_reserved1 :type 'bytes :length 8)
  (transparent-index :type 'byte)
  (_reserved2 :type 'bytes :length 3)
  (colors :type 'word)
  (pixel-width :type 'byte)
  (pixel-height :type 'byte)
  (grid-x :type 'word)
  (grid-y :type 'word)
  (grid-width :type 'word)
  (grid-height :type 'word)
  (_reserved3 :type 'bytes :length 84))

(define-binary-struct ase-binary-frame
  (bytes :type 'dword)
  (magic :type 'word)
  (old-chunks :type 'word)
  (duration :type 'word)
  (_reserved1 :type 'bytes :length 2)
  (new-chunks :type 'dword))

(define-binary-struct ase-binary-chunk-header
  (size :type 'dword)
  (type :type 'word))

(defun ase-binary-frame-chunks (frame)
  (if (zerop (ase-binary-frame-new-chunks frame))
      (ase-binary-frame-old-chunks frame)
      (ase-binary-frame-new-chunks frame)))

(defstruct ase-chunk
  (type nil :type symbol :read-only t))

(defstruct (ase-layer-chunk (:include ase-chunk (type 'layer)))
  (id 0 :type fixnum)
  (name "" :type string))

(defstruct ase-tag
  (from 0 :type fixnum)
  (to 0 :type fixnum)
  (name "" :type string))

(defstruct (ase-tags-chunk (:include ase-chunk (type 'tags)))
  (tags nil :type (vector ase-tag)))

(defstruct (ase-cel-chunk (:include ase-chunk (type 'cel)))
  (layer-id 0 :type fixnum)
  (data nil :type (vector (unsigned-byte 8))))

(defstruct ase-frame
  (duration 0 :type fixnum)
  (chunks nil :type (vector (or ase-chunk null))))

(defstruct ase-file
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (speed 0 :type fixnum)  ;; ms between frame
  (frames nil :type (vector ase-frame)))

(defgeneric read-chunk (type stream))

(defvar *layer-id*)
(declaim (type fixnum *layer-id*))

(defmethod read-chunk ((type (eql #x2004)) stream)
  (read-binary 'word stream) ;; flags
  (let ((type (read-binary 'word stream)))
    (declare (type fixnum type))
    (unless (zerop type)
      (error "group layers not supported")))
  (read-binary 'word stream) ;; child level
  (read-binary 'word stream) ;; default width
  (read-binary 'word stream) ;; default height
  (let ((blend-mode (read-binary 'word stream)))
    (declare (type fixnum blend-mode))
    (unless (zerop blend-mode)
      (error "layer blend modes not supported")))
  (read-binary 'byte stream) ;; opacity
  (read-binary 3 stream)
  (let ((name (read-binary 'ase-string stream)))
    (make-ase-layer-chunk
     :id (prog1 *layer-id* (incf *layer-id*))
     :name name)))

(declaim (ftype (function (virtual-binary-stream fixnum) (vector (unsigned-byte 8))) pass-data))
(defun pass-data (binary-stream size)
  (declare (ignore size))
  (let* ((buffer (slot-value binary-stream 'buffer))
         (position (slot-value binary-stream 'position))
         (stream-length (length (the vector buffer)))
         (stream-offset (the fixnum position))
         (input-size (- stream-length stream-offset)))
    (make-array
     input-size
     :element-type '(unsigned-byte 8)
     :displaced-to buffer
     :displaced-index-offset stream-offset)))

(defun decompress (binary-stream size)
  (let ((data (pass-data binary-stream size)))
    (chipz:decompress
     nil 'chipz:zlib
     (make-array
      (length data)
      :element-type '(unsigned-byte 8)
      :initial-contents data)
     :buffer-size size)))

(deftype sprite-dimension ()
  `(integer 0 ,(isqrt (truncate most-positive-fixnum 4))))

(defmethod read-chunk ((type (eql #x2005)) stream)
  (let ((layer-id (read-binary 'word stream)))
    (read-binary 'word stream)  ;; X position
    (read-binary 'word stream)  ;; Y position
    (read-binary 'byte stream)  ;; opacity
    (let ((process
            (ecase (read-binary 'word stream)
              (0 #'pass-data)
              (1 (error "linked cels not supported"))
              (2 #'decompress))))
      (read-binary 7 stream)
      (let ((width (the sprite-dimension (read-binary 'word stream)))
            (height (the sprite-dimension (read-binary 'word stream))))
        (make-ase-cel-chunk
         :layer-id layer-id
         :data (funcall process stream (* 4 width height)))))))

(defmethod read-chunk ((type (eql #x2018)) stream)
  (let* ((num-tags (read-binary 'word stream))
         (tags (make-array num-tags)))
    (read-binary 8 stream)
    (dotimes (tag-index num-tags)
      (let ((from (read-binary 'word stream))
            (to (read-binary 'word stream))
            (direction (read-binary 'byte stream))
            (dummy (read-binary 8 stream))
            (color (read-binary 3 stream))
            (extra (read-binary 'byte stream))
            (name (read-binary 'ase-string stream)))
        (declare (ignore direction dummy color extra))
        (setf (elt tags tag-index)
              (make-ase-tag :from from :to to :name name))))
    (make-ase-tags-chunk :tags tags)))

(defmethod read-chunk (type stream)
  (declare (ignore type stream))
  nil)

(defconstant +header-magic+ #xA5E0)
(defconstant +frame-magic+ #xF1FA)

(defun load-aseprite (stream)
  (let* ((*layer-id* 0)
         (header (read-binary 'ase-binary-header stream)))
    (unless (= (ase-binary-header-magic header) +header-magic+)
      (error "Invalid ASE file"))
    (unless (= (ase-binary-header-color-depth header) 32)
      (error "Only RGBA color mode supported"))
    (make-ase-file
     :width (ase-binary-header-width header)
     :height (ase-binary-header-height header)
     :speed (ase-binary-header-speed header)
     :frames
     (loop
       with frames-count = (ase-binary-header-frames header)
       with frames = (make-array frames-count)
       for frame-index below frames-count
       for frame = (let ((binary-frame (read-binary 'ase-binary-frame stream)))
                     (unless (= (ase-binary-frame-magic binary-frame) +frame-magic+)
                       (error "Invalid ASE frame"))
                     (make-ase-frame
                      :duration (ase-binary-frame-duration binary-frame)
                      :chunks
                      (loop
                        with chunks-count = (ase-binary-frame-chunks binary-frame)
                        with chunks = (make-array chunks-count)
                        for chunk-index below chunks-count
                        for chunk = (let ((chunk-header (read-binary
                                                         'ase-binary-chunk-header stream)))
                                      (read-chunk
                                       (ase-binary-chunk-header-type chunk-header)
                                       (make-instance
                                        'virtual-binary-stream
                                        :buffer
                                        (read-binary
                                         (the fixnum
                                              (- (ase-binary-chunk-header-size chunk-header)
                                                 6 ;; sizeof chunk-header
                                                 ))
                                         stream))))
                        do (setf (elt chunks chunk-index) chunk)
                        finally (return chunks))))
       do (setf (elt frames frame-index) frame)
       finally (return frames)))))
