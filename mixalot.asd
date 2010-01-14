(asdf:defsystem :mixalot
  :name "Mixalot mixer"
  :description "Mixalot mixer for ALSA"
  :version "0.0.2"
  :author "Andy Hefner <ahefner@gmail.com>"
  :license "MIT-style license"
  :depends-on (:cffi :bordeaux-threads :alexandria)
  :serial t
  :components ((:file "mixalot")))
