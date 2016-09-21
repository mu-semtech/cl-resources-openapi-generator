(asdf:defsystem :resources-openapi-generator
  :name "resources-openapi-generator"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Openapi generator for mu-cl-resources specification."
  :serial t
  :depends-on (mu-cl-resources)
  :components ((:file "packages")
               (:file "openapi-generator")))
