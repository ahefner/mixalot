;;; CFFI-Grovel is needed for processing grovel-file components
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :vorbisfile-ffi
  :name "libvorbisfile FFI"
  :description "CFFI interface to libvorbisfile"
  :version "0.0.1"
  :author "Sumant S.R. Oemrawsingh <soemraws@xs4all.nl>"
  :license "MIT-style license"
  :depends-on (:cffi)
  :serial t
  :components ((:file "vorbisfile-package")
               (cffi-grovel:grovel-file "vorbisfile-grovelling")
               (:file "vorbisfile")))
