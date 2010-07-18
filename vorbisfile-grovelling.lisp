;;;; CFFI grovelling file for libvorbisfile
;;;
;;; Sumant S.R. Oemrawsingh, 2010

(in-package :vorbisfile)

(include "vorbis/vorbisfile.h")
(cc-flags "-lvorbisfile")

;;;; Basic types

(cstruct oggvorbis-file "OggVorbis_File")

(cstruct vorbis-info "vorbis_info"
  (version "version" :type :int)
  (channels "channels" :type :int)
  (rate "rate" :type :long))

(cstruct vorbis-comment "vorbis_comment"
  (user-comments "user_comments" :type :pointer)
  (comment-lengths "comment_lengths" :type :pointer)
  (comments "comments" :type :int)
  (vendor "vendor" :type :string))
