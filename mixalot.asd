
(asdf:defsystem :mixalot
  :depends-on (:cffi :bordeaux-threads :alexandria)
  :serial t
  :components ((:file "mixalot")))
