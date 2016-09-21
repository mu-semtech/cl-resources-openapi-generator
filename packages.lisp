(defpackage :openapi-generator
  (:use :common-lisp)
  (:import-from :mu-cl-resources :s-url :s-var
                :ld-class)
  (:export :generate))
