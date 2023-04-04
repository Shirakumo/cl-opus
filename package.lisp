#|
 This file is a part of cl-opus
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.opus.cffi
  (:use #:cl)
  (:shadow #:close #:error)
  (:export
   #:libopus))

(defpackage #:org.shirakumo.fraf.opus
  (:use #:cl)
  (:local-nicknames
   (#:opus #:org.shirakumo.fraf.opus.cffi))
  (:shadow #:open #:close)
  (:export))
