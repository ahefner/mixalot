(defpackage :flac
  (:use :common-lisp :cffi)
  (:export #:flac-bool
           #:flac-int32
           #:flac-uint64

           #:flac-metadata
           #:flac-metadata-data
           #:flac-metadata-streaminfo
           #:flac-decoder-write-status
           #:flac-decoder-error-status

           #:metadataptr
           #:handleptr
           #:frameptr

           #:flac-strerror
           #:flac-error

           #:flac-decoder-new
           #:flac-decoder-delete

           #:flac-decoder-init-file
           #:flac-decoder-finish

           #:flac-decoder-process-until-end-of-metadata))
