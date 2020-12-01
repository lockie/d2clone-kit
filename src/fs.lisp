(in-package :d2clone-kit)

(cffi:define-foreign-library libphysfs
  (:windows nil)
  (:darwin (:or "libphysfs.3.0.2.dylib" "libphysfs.1.dylib"))
  (:unix (:or "libphysfs.so.3.0.2" "libphysfs.so.1"))
  (t (:default "libphysfs")))
(cffi:use-foreign-library libphysfs)

(cffi:defcfun ("PHYSFS_init" physfs-init) :int (argv0 :string))
(cffi:defcfun ("PHYSFS_deinit" physfs-deinit) :int)
(cffi:defcfun ("PHYSFS_getLastError" physfs-get-last-error) :string)
(cffi:defcfun ("PHYSFS_permitSymbolicLinks" physfs-permit-symbolic-links) :void
  (allow :int))
(cffi:defcfun ("PHYSFS_getBaseDir" physfs-get-base-dir) :string)
(cffi:defcfun ("PHYSFS_setWriteDir" physfs-set-write-dir) :int
  (new-dir :string))
(cffi:defcfun ("PHYSFS_enumerateFilesCallback" physfs-enumerate) :int
  (dir :string) (callback :pointer) (data :pointer))
(cffi:defcfun ("PHYSFS_getRealDir" physfs-get-real-dir) :string
  (file-name :string))
(cffi:defcfun ("PHYSFS_mount" physfs-mount) :int
  (new-dir :string) (mount-point :string) (append-to-path :int))

(defun mount (path &key (append t))
  (let ((path-string
          (if (typep path 'pathname)
              (namestring path)
              path)))
    (log-info "~a ~a to mount points"
              (if append "Appending" "Prepending")
              path-string)
    (when (zerop (physfs-mount path-string (cffi:null-pointer) (if append 1 0)))
      (log-warn "...failed: ~a" (physfs-get-last-error)))))

(declaim (type (function (string string)) *enumerate-directory-callback*))
(defvar *enumerate-directory-callback*)

(cffi:defcallback enumerate-directory-trampoline :int
    ((data :pointer) (directory :string) (file :string))
  (declare (ignore data))
  (funcall *enumerate-directory-callback* directory file))

(defmacro enumerate-directory (dir &body body)
  "Enumerates directory DIR. Executes BODY for each file with corresponding
variables DIRECTORY and FILE bound."
  (let ((callback-name (gensym "ENUMERATE-DIRECTORY-CALLBACK")))
    `(progn
       (defun ,callback-name (directory file)
         (declare (type string directory file) (ignorable directory file))
         ,@body
         1)
       (let ((*enumerate-directory-callback* #',callback-name))
         (physfs-enumerate ,dir
                           (cffi:callback enumerate-directory-trampoline)
                           (cffi:null-pointer))))))

(defunl init-fs (game-name data-dir)
  (when (zerop (physfs-init (first (uiop/image:raw-command-line-arguments))))
    (error "failed to initialize filesystem: ~a" (physfs-get-last-error)))
  (physfs-permit-symbolic-links 1)
  (when (zerop (physfs-set-write-dir (namestring data-dir)))
    (error "failed to initialize filesystem writing: ~a"
           (physfs-get-last-error)))
  (mount (merge-pathnames
          (truename
           (if (uiop:argv0)
               (merge-pathnames "../" (directory-namestring (uiop:argv0)))
               "../"))
          "Resources"))
  (mount data-dir)
  (dolist (dir (uiop/configuration:xdg-data-dirs))
    (mount
     (merge-pathnames
      (make-pathname :directory `(:relative ,game-name))
      dir)))
  (enumerate-directory "/"
    (when (uiop:string-suffix-p file ".zip")
      (mount (merge-pathnames (uiop:ensure-directory-pathname
                               (pathname (physfs-get-real-dir file)))
                              (pathname file)))))
  (al:set-physfs-file-interface))

(defunl close-fs ()
  (when (zerop (physfs-deinit))
    (log-error "failed to close filesystem: ~a" (physfs-get-last-error))))

(declaim (inline ensure-loaded))
(defun ensure-loaded (load-fn file-name &rest rest)
  "Calls LOAD-FN (which could be #'AL:LOAD-BITMAP, #'AL:LOAD-SAMPLE or
similar) with the FILE-NAME argument and REST arguments, if any.  If the
result of calling of LOAD-FN is null pointer, then the error is raised.
Otherwise, it is returned."
  ;; TODO : restarts to get desired filename
  (let ((file (apply load-fn file-name rest)))
    (if (cffi:null-pointer-p file)
        (error "failed to open '~a'" file-name)
        file)))

(declaim
 (inline sanitize-filename)
 (ftype (function (string) string) sanitize-filename))
(defun sanitize-filename (filename)
  (values (cl-ppcre:regex-replace-all
           "\\x22|\\x2a|\\x2f|\\x3a|\\x3c|\\x3e|\\x3f|\\x5c|\\x7c" filename
           "")))

(declaim (ftype (function (string) list) read-file-into-list))
(defun read-file-into-list (pathname)
  "Reads text file specified by PATHNAME into a list line-by-line.
Lines are expected to be shorter than 4k chars."
  (cffi:with-foreign-object (buffer :char 4096)
    (loop
      :with file := (al:fopen (namestring pathname) "r")
        :initially (when (cffi:null-pointer-p file) (return nil))
      :for line := (al:fgets file buffer 4096)
      :until (cffi:null-pointer-p line)
      :collecting
      (flet ((trim (str) (subseq str 0 (1- (length str)))))
        (trim (cffi:foreign-string-to-lisp line)))
      :finally (al:fclose file))))

(defclass character-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((path :initarg :path :initform (error "missing path"))
   (al-file))
  (:documentation "Wrapper around liballegro
[file APIs](https://liballeg.org/a5docs/trunk/file.html)."))

(defmethod initialize-instance :after ((stream character-stream) &key)
  (with-slots (path al-file) stream
    (setf al-file (al:fopen path "r"))
    (when (cffi:null-pointer-p al-file)
      (error "failed to open '~a'" path))))

(defmethod trivial-gray-streams:stream-read-char ((stream character-stream))
  (with-slots (path al-file) stream
    (let ((char (al:fgetc al-file)))
      (if (minusp char)
          (if (al:feof al-file)
              :eof
              (error "error reading '~a': ~a" path (al:ferrmsg al-file)))
          (code-char char)))))

;; TODO
;; (defmethod stream-read-sequence ((stream character-stream) sequence
;;                                  start end &key &allow-other-keys)
;; ;;   (with-slots (path al-file) stream
;; ;;     (let ((length (- end start)
;; ;;     (cffi:with-foreign-string
;; ;;     (al:fread
;;   )

(defmethod trivial-gray-streams:stream-unread-char ((stream character-stream)
                                                    char)
  (with-slots (path al-file) stream
    (al:fungetc al-file (char-code char))))

(defclass binary-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((path :initarg :path :initform (error "missing path"))
   (al-file))
  (:documentation "Wrapper around liballegro
[file APIs](https://liballeg.org/a5docs/trunk/file.html)."))

(defmethod initialize-instance :after ((stream binary-stream) &key)
  (with-slots (path al-file) stream
    (setf al-file (al:fopen path "r"))
    (when (cffi:null-pointer-p al-file)
      (error "failed to open '~a'" path))))

(defmethod stream-element-type ((stream binary-stream))
  '(unsigned-byte))

(defmethod trivial-gray-streams:stream-read-byte ((stream binary-stream))
  (with-slots (path al-file) stream
    (let ((char (al:fgetc al-file)))
      (if (minusp (the fixnum char))
          (if (al:feof al-file)
              :eof
              (error "error reading '~a': ~a" path (al:ferrmsg al-file)))
          char))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream binary-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (declare (type non-negative-fixnum start end))
  (with-slots (path al-file) stream
    (cffi:with-pointer-to-vector-data (buffer sequence)
      (let ((pointer (cffi:inc-pointer buffer start)))
        (+ start (al:fread al-file pointer (- end start)))))))

(defclass virtual-binary-stream
    (trivial-gray-streams:fundamental-binary-input-stream)
  ((buffer :initarg :buffer :initform (error "missing buffer"))
   (position :initform 0))
  (:documentation "Read-only binary Gray stream based on SIMPLE-ARRAY of
  UNSIGNED-BYTE."))

(defmethod stream-element-type ((stream virtual-binary-stream))
  '(unsigned-byte))

(defmethod trivial-gray-streams:stream-read-byte ((stream
                                                   virtual-binary-stream))
  (with-slots (buffer position) stream
    (declare (type (simple-array (unsigned-byte 8)) buffer)
             (type non-negative-fixnum position))
    (elt buffer (prog1 position (incf position)))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream
                                                       virtual-binary-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (with-slots (buffer position) stream
    (declare (type (simple-array (unsigned-byte 8)) sequence buffer)
             (type non-negative-fixnum position start end))
    (replace sequence buffer :start1 start :end1 end :start2 position)
    (let ((copied (min (- end start) (- (length buffer) position))))
      (+ start (incf position copied)))))

(defgeneric read-binary (type stream)
  (:documentation "Reads and returns element of type denoted by TYPE from
  binary stream STREAM."))

(defmethod read-binary ((type (eql 'byte)) stream)
  (read-byte stream))

(defmethod read-binary ((type (eql 'word)) stream)
  (let ((word 0))
    (setf (ldb (byte 8 0) word) (read-byte stream))
    (setf (ldb (byte 8 8) word) (read-byte stream))
    word))

(defmethod read-binary ((type (eql 'dword)) stream)
  (let ((dword 0))
    (setf (ldb (byte 8 0) dword) (read-byte stream))
    (setf (ldb (byte 8 8) dword) (read-byte stream))
    (setf (ldb (byte 8 16) dword) (read-byte stream))
    (setf (ldb (byte 8 24) dword) (read-byte stream))
    dword))

(defmethod read-binary ((length fixnum) stream)
  (let ((string (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence string stream)
    string))

(defmacro define-binary-struct (name &rest slots)
  "Defines structure with name NAME and slots SLOTS along with corresponding
READ-BINARY method which reads and returns that structure from given binary
stream.

See READ-BINARY"
  (flet
      ((slot->struct-slot (spec)
         (let* ((parameters (cdr spec))
                (type (eval (getf parameters :type)))
                (lisp-type (ecase type
                             ((byte word dword) 'fixnum)
                             (string 'string)
                             (bytes 't)))
                (initial (ecase type
                           ((byte word dword) 0)
                           (string "")
                           (bytes nil))))
           `(,(car spec) ,initial :type ,lisp-type :read-only t)))
       (slot->slot-ctor (spec)
         (let* ((parameters (cdr spec))
                (type (getf parameters :type))
                (length (getf parameters :length)))
           `(,(make-keyword (car spec))
             (read-binary ,(if length length type) stream)))))
    `(progn
       (defstruct ,name
         ,@(mapcar #'slot->struct-slot slots))
       (defmethod read-binary ((type (eql ',name)) stream)
         (,(symbolicate :make- name)
          ,@(mapcan #'slot->slot-ctor slots))))))
