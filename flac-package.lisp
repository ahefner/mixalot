(defpackage :flac
  (:use :common-lisp :cffi :mixalot-strings-common)
  (:export #:flac-bool
           #:flac-int16
           #:flac-int32
           #:flac-uint64

           #:flac-strerror
           #:flac-error
           #:flac-error-status

           #:flac-open
           #:flac-close
           #:flac-eof
           #:flac-read
           #:flac-seek
           #:flac-tell
           #:flac-process-metadata

           #:flac-decoder
           #:flac-decoder-handle
           #:flac-num-channels
           #:flac-metadata
           #:flac-raw-metadata
           #:flac-stream-position

           #:get-flac-tags-from-file
           #:get-flac-tags-from-handle))
