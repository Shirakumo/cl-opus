#|
 This file is a part of cl-opus
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.opus)

(define-condition opus-error (error)
  ((file :initarg :file :reader file)
   (code :initarg :code :reader code))
  (:report (lambda (c s) (format s "The opus operation failed with the following error:~%  ~a"
                                 (code c)))))

(defvar *init* NIL)
(defun init ()
  (unless *init*
    (cffi:load-foreign-library 'opus:libopus)
    (setf *init* T)))

(defun shutdown ()
  (when *init*
    (cffi:close-foreign-library 'opus:libopus)
    (setf *init* NIL)))

(defmacro with-pinned-buffer ((ptr data &key (offset 0)) &body body)
  (let ((datag (gensym "DATA")) (thunk (gensym "THUNK")))
    `(let ((,datag ,data))
       (flet ((,thunk (,ptr)
                (declare (type cffi:foreign-pointer ,ptr))
                ,@body))
         (cond #+sbcl
               ((typep ,datag 'sb-kernel:simple-unboxed-array)
                (sb-sys:with-pinned-objects (,datag)
                  (let ((,ptr (sb-sys:vector-sap ,datag)))
                    (,thunk (cffi:inc-pointer ,ptr ,offset)))))
               (T
                (,thunk (static-vectors:static-vector-pointer ,datag :offset ,offset))))))))

(declaim (inline check-return))
(defun check-return (file return)
  (etypecase return
    (cffi:foreign-pointer
     (if (cffi:null-pointer-p return)
         (error 'opus-error :file file :code :false)
         return))
    (symbol
     (case return
       (:ok
        return)
       (T
        (error 'opus-error :file file :code return))))
    (integer
     (if (<= 0 return)
         return
         (error 'opus-error :file file :code (cffi:foreign-enum-keyword 'opus:error return))))))

(defstruct (file
            (:conc-name NIL)
            (:constructor %make-file (handle &key channels sample-count read-fun seek-fun index-fun close-fun))
            (:copier NIL)
            (:predicate NIL))
  (handle NIL :type cffi:foreign-pointer)
  (channels 0 :type (unsigned-byte 8) :read-only T)
  (sample-count 0 :type (unsigned-byte 64) :read-only T)
  (read-fun (constantly -1) :type function)
  (seek-fun (constantly -1) :type function)
  (index-fun (constantly -1) :type function)
  (close-fun (constantly -1) :type function))

(defun make-file (handle error &rest args)
  (check-return NIL (cffi:mem-ref error 'opus:error))
  (let ((length (opus:pcm-total handle -1)))
    (apply #'%make-file handle
           :channels (opus:channel-count handle -1)
           :sample-count (if (<= 0 length) length 0)
           args)))

(defun close (file)
  (opus:free (handle file))
  (setf (handle file) (cffi:null-pointer)))

(defun open-file (path &key)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :ok)
    (make-file (opus:open-file (namestring (truename path)) error) error)))

(defun open-pointer (ptr &key (start 0) end)
  (cffi:incf-pointer ptr start)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :ok)
    (make-file (opus:open-memory ptr (- end start) error) error)))

(defun open-vector (vector &key (start 0) end)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :ok)
    (make-file (opus:open-memory (static-vectors:static-vector-pointer vector :offset start) (- end start) error) error)))

(defvar *callback-counter* 0)
(defvar *callbacks* (make-hash-table :test 'eql))

(defmacro define-callback (name retval args &body body)
  `(cffi:defcallback ,name ,retval ((file-idx :size) ,@args)
     (handler-case 
         (let ((file (gethash file-idx *callbacks*)))
           (when file
             ,@body
             (funcall (,name file) file ,@(mapcar #'first args))))
       (error () -1))))

(define-callback read-fun :int ((buffer :pointer) (bytes :int)))
(define-callback seek-fun :int ((offset :int64) (whence :int)))
(define-callback index-fun :int64 ())
(define-callback close-fun :int ()
  (remhash file-idx *callbacks*))

(defun open-callback (read-fun &key seek-fun index-fun close-fun
                                    initial-data initial-data-start initial-data-end)
  (cffi:with-foreign-objects ((error 'opus:error)
                              (callbacks '(:struct opus:callbacks)))
    (setf (opus:callbacks-read callbacks) (cffi:callback read-fun))
    (setf (opus:callbacks-seek callbacks) (cffi:callback seek-fun))
    (setf (opus:callbacks-tell callbacks) (cffi:callback index-fun))
    (setf (opus:callbacks-close callbacks) (cffi:callback close-fun))
    (let ((data (if initial-data
                    (static-vectors:static-vector-pointer initial-data :offset (or initial-data-start 0))
                    (cffi:null-pointer)))
          (size (if initial-data (- (or initial-data-end (length initial-data)) (or initial-data-start 0)) 0))
          (index (incf *callback-counter*)))
      (make-file (opus:open-callbacks (cffi:make-pointer index) callbacks data size error)
                 :read-fun read-fun
                 :seek-fun (or seek-fun (constantly -1))
                 :index-fun (or index-fun (constantly -1))
                 :close-fun (or close-fun (constantly 0))))))

(defun open (thing &rest initargs &key &allow-other-keys)
  (init)
  (etypecase thing
    ((or string pathname)
     (apply #'open-file thing initargs))
    (cffi:foreign-pointer
     (apply #'open-pointer thing initargs))
    ((simple-array (unsigned-byte 8) (*))
     (apply #'open-vector thing initargs))
    (function
     (apply #'open-callback thing initargs))))

(defmacro with-file ((file input &rest args) &body body)
  (let ((fileg (gensym "FILE")))
    `(let* ((,fileg (open ,input ,@args))
            (,file ,fileg))
       (unwind-protect
            (progn ,@body)
         (close ,fileg)))))

(defun samplerate (file)
  (declare (ignore file))
  ;; libopusfile fixes the rate to 48kHz on output
  48000)

(defun duration (file)
  (/ (sample-count file) 48000.0))

(defun seekable-p (file)
  (opus:seekable-p (handle file)))

(defun link-count (file)
  (opus:link-count (handle file)))

(defun serial-number (file &optional link)
  (opus:serial-number (handle file) (or link -1)))

(defun current-link (file)
  (opus:current-link (handle file)))

(defun bitrate (file &optional link)
  (check-return file (opus:bitrate (handle file) (or link -1))))

(defun instant-bitrate (file)
  (check-return file (opus:bitrate-instant (handle file))))

(defun index (file)
  (check-return file (opus:pcm-tell (handle file))))

(defun (setf index) (index file)
  (check-return file (opus:pcm-seek (handle file) index))
  index)

(defun seek (file to &key (by :second) (mode :absolute))
  (ecase mode
    (:absolute
     (ecase by
       (:second
        (check-return file (opus:pcm-seek (handle file) (truncate (* 48000 to)))))
       (:sample
        (check-return file (opus:pcm-seek (handle file) to)))
       (:byte
        (check-return file (opus:raw-seek (handle file) to)))))
    (:relative
     (ecase by
       (:second
        (check-return file (opus:pcm-seek (handle file) (+ (index file) (truncate (* 48000 to))))))
       (:sample
        (check-return file (opus:pcm-seek (handle file) (+ (index file) to))))
       (:byte
        (check-return file (opus:raw-seek (handle file) (+ (opus:raw-tell (handle file)) to)))))))
  file)

(defun comments (file &optional link)
  (let ((tags (check-return file (opus:tags (handle file) (or link -1)))))
    (loop for i from 0 below (opus:tags-comments tags)
          for string = (cffi:mem-aref (opus:tags-user-comments tags) :pointer i)
          for length = (cffi:mem-aref (opus:tags-comment-lengths tags) :int i)
          collect (cffi:foreign-string-to-lisp string :count length))))

(defun vendor (file &optional link)
  (let ((tags (check-return file (opus:tags (handle file) (or link -1)))))
    (opus:tags-vendor tags)))

(defun gain (file &optional (type :header))
  (ecase type
    (:header (* (opus:head-output-gain (opus:head (handle file) -1))
                (/ 1.0 256.0)))))

(defun (setf gain) (gain file &optional (type :header))
  (let ((gain-int (round (* 256.0 gain))))
    (check-return file (opus:set-gain-offset (handle file) type gain-int)))
  gain)

(defun decode-float (file buffer &key (start 0) end stereo)
  (let ((byte-stride (* 4 (if stereo 2 (channels file)))))
    (with-pinned-buffer (pointer buffer :offset (* start byte-stride))
      (let* ((length (* (- end start) byte-stride))
             (samples (if stereo
                          (opus:read-stereo-float (handle file) pointer length)
                          (opus:read-float (handle file) pointer length (cffi:null-pointer)))))
        (check-return file samples)
        (+ start (* samples (if stereo 2 (channels file))))))))

(defun decode-int16 (file buffer &key (start 0) end stereo)
  (let ((byte-stride (* 2 (if stereo 2 (channels file)))))
    (with-pinned-buffer (pointer buffer :offset (* start byte-stride))
      (let* ((length (* (- end start) byte-stride))
             (samples (if stereo
                          (opus:read-stereo (handle file) pointer length)
                          (opus:read (handle file) pointer length (cffi:null-pointer)))))
        (check-return file samples)
        (+ start (* samples (if stereo 2 (channels file))))))))
