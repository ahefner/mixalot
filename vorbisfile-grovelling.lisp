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

(constant (OV_FALSE "OV_FALSE"))
(constant (OV_EOF "OV_EOF"))
(constant (OV_HOLE "OV_HOLE"))

(constant (OV_EREAD "OV_EREAD"))
(constant (OV_EFAULT "OV_EFAULT"))
(constant (OV_EIMPL "OV_EIMPL"))
(constant (OV_EINVAL "OV_EINVAL"))
(constant (OV_ENOTVORBIS "OV_ENOTVORBIS"))
(constant (OV_EBADHEADER "OV_EBADHEADER"))
(constant (OV_EVERSION "OV_EVERSION"))
(constant (OV_ENOTAUDIO "OV_ENOTAUDIO"))
(constant (OV_EBADPACKET "OV_EBADPACKET"))
(constant (OV_EBADLINK "OV_EBADLINK"))
(constant (OV_ENOSEEK "OV_ENOSEEK"))
