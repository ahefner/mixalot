(asdf:defsystem :mpg123-ffi
  :name "libmpg123 FFI"
  :description "CFFI interface to libmpg123"
  :version "0.0.1"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi)
  :serial t
  :components ((:file "mpg123")))
