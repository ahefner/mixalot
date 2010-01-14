(asdf:defsystem :mixalot-mp3
  :name "Mixalot-MP3"
  :description "MP3 Streamer class for Mixalot"
  :version "0.0.1"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi :mixalot :mpg123-ffi)
  :serial t
  :components ((:file "mp3-stream")))
