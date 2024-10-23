;;;; CFFI grovelling file for libFLAC metadata
;;;
;;; Sumant S.R. Oemrawsingh, 2010

(in-package :flac)

(include "FLAC/stream_decoder.h")
(cc-flags "-lFLAC" "-lm"
          #+freebsd "-I/usr/local/include"
          #+freebsd "-L/usr/local/lib"
          #+darwin "-I/opt/local/include"
          #+darwin "-L/opt/local/lib")

(cstruct flac-metadata "FLAC__StreamMetadata"
  (type "type" :type flac-metadata-type)
  (data "data" :type flac-metadata-data))
