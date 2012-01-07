;;; CFFI-Grovel is needed for processing grovel-file components
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :flac
  :name "libFLAC FFI"
  :description "CFFI interface to libFLAC"
  :version "0.0.2"
  :author "Sumant S.R. Oemrawsingh <soemraws@xs4all.nl>, Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi)
  :serial t
  :components ((:file "strings-common")
               (:file "flac-package")
               (cffi-grovel:grovel-file "flac-grovelling")
               (:file "flac-metadata")
               (cffi-grovel:grovel-file "flac-grovelling-metadata")
               (:file "flac")))
