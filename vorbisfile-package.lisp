(defpackage :vorbisfile
  (:use :common-lisp :cffi)
  (:export #:open-oggvorbis-file
           #:close-oggvorbis-file
           #:get-vorbis-channels
           #:get-vorbis-rate
           #:get-vorbis-comments
           #:get-vorbis-comment-from-comments
           #:get-vorbis-comment))
