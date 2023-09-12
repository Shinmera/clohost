(asdf:defsystem clohost
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A client library for the Cohost API"
  :homepage "https://shinmera.github.io/clohost/"
  :bug-tracker "https://github.com/shinmera/clohost/issues"
  :source-control (:git "https://github.com/shinmera/clohost.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "client")
               (:file "objects")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :crypto-shortcuts
               :com.inuoe.jzon
               :drakma))
