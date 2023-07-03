(asdf:defsystem cl-opus
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
