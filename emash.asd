(asdf:defsystem emash
  :name "emash"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "an e-mail shell"
  :licence "MIT"
  :depends-on ("cffi" "whirlog")
  :serial t
  :components ((:file "emash")))
