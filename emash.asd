(asdf:defsystem emash
  :name "emash"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "an e-mail shell"
  :licence "MIT"
  :build-operation "asdf:program-op"
  :build-pathname "emash"
  :entry-point "emash:start"
  :depends-on ("cffi" "whirlog")
  :serial t
  :components ((:file "util") (:file "curl") (:file "smtp") (:file "emash")))
