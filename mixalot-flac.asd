(asdf:defsystem :mixalot-flac
  :name "Mixalot-FLAC"
  :description "FLAC Streamer class for Mixalot"
  :version "0.0.1"
  :author "Sumant S.R. Oemrawsingh <soemraws@xs4all.nl>"
  :license "MIT-style license"
  :depends-on (:cffi :mixalot :flac-ffi)
  :serial t
  :components ((:file "flac-stream")))
