(asdf:defsystem :mixalot-flac
  :name "Mixalot-FLAC"
  :description "FLAC Streamer class for Mixalot"
  :version "0.0.2"
  :author "Sumant S.R. Oemrawsingh <soemraws@xs4all.nl>, Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi :mixalot :flac)
  :serial t
  :components ((:file "flac-stream")))
