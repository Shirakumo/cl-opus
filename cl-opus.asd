#|
 This file is a part of cl-opus
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-opus
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Bindings to libopusfile, a simple and free OGG/Opus decoding library"
  :homepage "https://Shirakumo.github.io/cl-opus/"
  :bug-tracker "https://github.com/Shirakumo/cl-opus/issues"
  :source-control (:git "https://github.com/Shirakumo/cl-opus.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:cffi
               :trivial-features
               :trivial-garbage
               :static-vectors
               :documentation-utils))
