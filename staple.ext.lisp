(asdf:load-system :staple-markless)

(defpackage "opus-docs"
  (:use #:cl)
  (:local-nicknames
   (#:opus #:org.shirakumo.fraf.opus)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "opus-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :cl-opus))))
  'page*)
