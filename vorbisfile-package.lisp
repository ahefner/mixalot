(defpackage :vorbisfile
  (:use :common-lisp :cffi)
  (:export #:oggvorbis-strerror
           #:check-oggvorbis-error
           #:check-oggvorbis-pointer-error
           #:raise-oggvorbis-error

           #:oggvorbis-new
           #:oggvorbis-delete
           #:oggvorbis-open
           #:oggvorbis-close
           #:get-oggvorbis-channels
           #:get-oggvorbis-length
           #:get-oggvorbis-rate
           
           #:ov-read))
