(asdf:defsystem :mixalot-vorbis
  :name "Mixalot-vorbis"
  :description "Vorbis Streamer class for Mixalot"
  :version "0.0.1"
  :author "Sumant S.R. Oemrawsingh <soemraws@xs4all.nl>"
  :license "MIT-style license"
  :depends-on (:cffi :mixalot :vorbisfile-ffi)
  :serial t
  :components ((:file "vorbis-stream")))
