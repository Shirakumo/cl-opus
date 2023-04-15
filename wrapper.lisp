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
            (:constructor %make-file (handle &key read-fun seek-fun index-fun close-fun))
            (:copier NIL)
            (:predicate NIL))
  (handle NIL :type cffi:foreign-pointer)
  (read-fun (constantly -1) :type function)
  (seek-fun (constantly -1) :type function)
  (index-fun (constantly -1) :type function)
  (close-fun (constantly -1) :type function)
  (user-data NIL :type T))

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type T)
    (if (cffi:null-pointer-p (handle file))
        (format stream "CLOSED")
        (let ((i (* (index file) (/ 48000.0)))
              (l (* (max 0 (opus:pcm-total (handle file) -1)) (/ 48000.0))))
          (if (< 0 l)
              (format stream "~d:~2,'0d / ~d:~2,'0d (~d%)"
                      (floor i 60) (floor (mod i 60))
                      (floor l 60) (floor (mod i 60))
                      (round (/ i l)))
              (format stream "~d:~2,'0d"
                      (floor i 60) (floor (mod i 60))))))))

(defmethod describe-object ((file file) stream)
  (let ((i (/ (opus:pcm-tell (handle file)) 48000.0))
        (l (/ (opus:pcm-total (handle file) -1) 48000.0)))
    (format stream "~a

Position:~20t~:[Unknown~;~:*~{~d:~2,'0d~}~]
Duration:~20t~:[Unknown~;~:*~{~d:~2,'0d~}~]
Sample count:~20t~:[~a~;Unknown~]
Samplerate:~20t~a
Bitrate:~20t~a
Channels:~20t~a
Seekable:~20t~:[Yes~;No~]
Link:~20t~{~d/~d~}
Serial number:~20t~a
Vendor:~20t~a
Comments:~20t~{~a~^~%~20t~}"
            file
            (when (<= 0 i) (list (floor i 60) (floor (mod i 60))))
            (when (<= 0 l) (list (floor l 60) (floor (mod l 60))))
            (when (<= 0 l) (opus:pcm-total (handle file) -1))
            (samplerate file)
            (bitrate file)
            (channels file)
            (seekable-p file)
            (list (current-link file) (link-count file))
            (serial-number file)
            (vendor file)
            (comments file))))

(defun make-file (handle error &rest args)
  (check-return NIL (cffi:mem-ref error 'opus:error))
  (apply #'%make-file handle args))

(defun close (file)
  (opus:free (handle file))
  (setf (handle file) (cffi:null-pointer)))

(defun open-file (path &key)
  (init)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :ok)
    (make-file (opus:open-file (namestring (truename path)) error) error)))

(defun open-pointer (ptr &key (start 0) end)
  (init)
  (cffi:incf-pointer ptr start)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :ok)
    (make-file (opus:open-memory ptr (- end start) error) error)))

(defun open-vector (vector &key (start 0) end)
  (init)
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
             (funcall (,name file) (user-data file) ,@(mapcar #'first args))))
       (error () -1))))

(define-callback read-fun :int ((buffer :pointer) (bytes :int)))
(define-callback seek-fun :int ((offset :int64) (whence opus:whence)))
(define-callback index-fun :int64 ())
(define-callback close-fun :int ()
  (remhash file-idx *callbacks*))

(defun open-callback (read-fun &key seek-fun index-fun close-fun user-data
                                    initial-data initial-data-start initial-data-end)
  (init)
  (cffi:with-foreign-objects ((error 'opus:error)
                              (callbacks '(:struct opus:callbacks)))
    (setf (opus:callbacks-read callbacks) (cffi:callback read-fun))
    (setf (opus:callbacks-seek callbacks) (cffi:callback seek-fun))
    (setf (opus:callbacks-tell callbacks) (cffi:callback index-fun))
    (setf (opus:callbacks-close callbacks) (cffi:callback close-fun))
    (let* ((data (etypecase initial-data
                   (cffi:foreign-pointer
                    initial-data)
                   ((simple-array (unsigned-byte 8) (*))
                    (static-vectors:static-vector-pointer initial-data :offset (or initial-data-start 0)))
                   (null
                    (cffi:null-pointer))))
           (size (etypecase initial-data
                   (cffi:foreign-pointer
                    (- initial-data-end (or initial-data-start 0)))
                   ((simple-array (unsigned-byte 8) (*))
                    (- (or initial-data-end (length initial-data)) (or initial-data-start 0)))
                   (null
                    0)))
           (index (incf *callback-counter*))
           (file (make-file (opus:open-callbacks (cffi:make-pointer index) callbacks data size error)
                            :read-fun read-fun
                            :seek-fun (or seek-fun (constantly -1))
                            :index-fun (or index-fun (constantly -1))
                            :close-fun (or close-fun (constantly 0))
                            :user-data user-data)))
      (unless user-data
        (setf (user-data file) file))
      file)))

(defstruct (stream-wrapper
            (:constructor make-stream-wrapper (stream offset)))
  (stream NIL :type stream)
  (offset 0 :type (unsigned-byte 64)))

(defun stream-read (wrapper out bytes)
  (let* ((stream (stream-wrapper-stream wrapper))
         (in (make-array 4096 :element-type '(unsigned-byte 8)))
         (to-read bytes))
    (declare (dynamic-extent in))
    (with-pinned-buffer (ptr in)
      (loop for read = (read-sequence in stream :end (min 4096 to-read))
            while (< 0 read)
            do (static-vectors:replace-foreign-memory out ptr read)
               (cffi:incf-pointer out read)
               (decf to-read read)))
    (let ((read (- bytes to-read)))
      (incf (stream-wrapper-offset wrapper) read)
      read)))

(defun stream-seek (wrapper offset whence)
  (let* ((stream (stream-wrapper-stream wrapper))
         (position (ecase whence
                     (:start offset)
                     (:cur (+ (file-position stream) offset))
                     (:end (- (file-length stream) offset)))))
    (cond ((file-position stream position)
           (setf (stream-wrapper-offset wrapper) position)
           1)
          (T 0))))

(defun stream-index (wrapper)
  (stream-wrapper-offset wrapper))

(defun stream-close (wrapper)
  (close (stream-wrapper-stream wrapper))
  1)

(defun open-stream (stream &rest args &key initial-data initial-data-start initial-data-end)
  (declare (ignorable initial-data initial-data-start initial-data-end))
  (etypecase stream
    (file-stream
     (apply #'open-callback #'stream-read
            :seek-fun #'stream-seek
            :index-fun #'stream-index
            :close-fun #'stream-close
            :user-data (make-stream-wrapper stream (file-position stream))
            :inital-data initial-data
            args))
    (stream
     (apply #'open-callback #'stream-read
            :index-fun #'stream-index
            :close-fun #'stream-close
            :user-data (make-stream-wrapper stream (- (or initial-data-end 0) (or initial-data-start 0)))
            args))))

(defun open (thing &rest initargs &key &allow-other-keys)
  (etypecase thing
    ((or string pathname)
     (apply #'open-file thing initargs))
    (cffi:foreign-pointer
     (apply #'open-pointer thing initargs))
    ((simple-array (unsigned-byte 8) (*))
     (apply #'open-vector thing initargs))
    (function
     (apply #'open-callback thing initargs))
    (stream
     (apply #'open-stream thing initargs))))

(defmacro with-file ((file input &rest args) &body body)
  (let ((fileg (gensym "FILE")))
    `(let* ((,fileg (open ,input ,@args))
            (,file ,fileg))
       (unwind-protect
            (progn ,@body)
         (close ,fileg)))))

(declaim (inline samplerate channels sample-count duration
                 seekable-p link-count serial-number current-link
                 bitrate instant-bitrate index (setf index) seek
                 gain (setf gain)))
(defun samplerate (file)
  (declare (ignore file))
  ;; libopusfile fixes the rate to 48kHz on output
  48000)

(defun channels (file &optional link)
  (check-return file (opus:channel-count (handle file) (or link -1))))

(defun sample-count (file &optional link)
  (check-return file (opus:pcm-total (handle file) (or link -1))))

(defun duration (file &optional link)
  (/ (sample-count file link) 48000.0))

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
