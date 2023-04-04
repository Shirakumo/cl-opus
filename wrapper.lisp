#|
 This file is a part of cl-opus
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.opus)

(define-condition need-more-data (warning)
  ())

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
                    (,thunk (cffi:inc-pointer ,ptr (* ,offset 4))))))
               (T
                (,thunk (static-vectors:static-vector-pointer ,datag :offset (* ,offset 4)))))))))

(defun check-error (file error)
  (case error
    (:no-error
     NIL)
    (:need-more-data
     (warn 'need-more-data))
    (T
     (error 'opus-error :file file :code error))))

(defstruct (file
            (:conc-name NIL)
            (:constructor %make-file (handle channels samplerate max-frame-size))
            (:copier NIL)
            (:predicate NIL))
  (handle NIL :type cffi:foreign-pointer)
  (channels 0 :type (unsigned-byte 8) :read-only T)
  (samplerate 0 :type (unsigned-byte 32) :read-only T)
  (max-frame-size 0 :type (unsigned-byte 32) :read-only T))

(defun make-file (handle error)
  (check-error NIL (cffi:mem-ref error 'opus:error))
  (cffi:with-foreign-objects ((info '(:struct opus:info)))
    (opus:get-info handle info)
    (%make-file handle (opus:info-channels info) (opus:info-samplerate info) (opus:info-max-frame-size info))))

(defun check-file-for-error (file)
  (check-error file (opus:get-error (handle file))))

(defun close (file)
  (opus:close (handle file))
  (setf (handle file) (cffi:null-pointer)))

(defun open (thing &rest initargs &key buffer start end)
  (declare (ignore buffer start end))
  (init)
  (etypecase thing
    ((or string pathname)
     (apply #'open-file thing initargs))
    (cffi:foreign-pointer
     (apply #'open-pointer thing initargs))
    ((simple-array (unsigned-byte 8) (*))
     (apply #'open-vector thing initargs))))

(defun open-file (path &key buffer)
  (cffi:with-foreign-objects ((error 'opus:error))
    (setf (cffi:mem-ref error 'opus:error) :no-error)
    (make-file (opus:open-filename (namestring (truename path)) error (or buffer (cffi:null-pointer))) error)))

(defmacro with-file ((file input &rest args) &body body)
  (let ((fileg (gensym "FILE")))
    `(let* ((,fileg (open ,input ,@args))
            (,file ,fileg))
       (unwind-protect
            (progn ,@body)
         (close ,fileg)))))
