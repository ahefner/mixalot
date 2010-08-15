(defpackage :flac
  (:use :common-lisp :cffi)
  (:export #:flac-bool
           #:flac-int16
           #:flac-int32
           #:flac-uint64

           #:flac-strerror
           #:flac-error

           #:flac-open
           #:flac-close
           #:flac-eof
           #:flac-read
           #:flac-seek
           #:flac-tell
           #:flac-process-metadata

           #:flac-decoder-handle
           #:flac-client-data

           #:flac-client-data-metadata
           #:flac-client-data-buffer
           #:flac-client-data-buffer-size
           #:flac-client-data-block-size))
