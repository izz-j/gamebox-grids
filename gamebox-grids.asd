(in-package :cl-user)

(asdf:defsystem #:gamebox-grids
  :description "Create and manipulate tiles in a two-dimensional grid layout."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :maintainer "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/gamebox-grids"
  :bug-tracker "https://github.com/mfiano/gamebox-grids/issues"
  :source-control (:git "git@github.com:mfiano/gamebox-grids.git")
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:alexandria
               #:gamebox-math)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "grid")
   (:file "quad")
   (:file "hex")))
