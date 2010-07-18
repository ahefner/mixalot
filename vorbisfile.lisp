(in-package :vorbisfile)

(define-foreign-library libvorbisfile
  (:unix (:or "libvorbisfile.so.3"
              "/usr/lib/libvorbisfile.so"
              "/usr/local/lib/libvorbisfile.so"))
  (t (:default "libvorbisfile")))

(use-foreign-library libvorbisfile)

;;;; Basic types

(defctype handleptr :pointer)


;;;; Interface to libvorbisfile (added as needed)

;;; Setup/Teardown

(defcfun "ov_fopen" :int
  (path :string)
  (vf handleptr))

(defcfun "ov_clear" :int
  (vf handleptr))


;;; Decoding

(defcfun "ov_read" :long
  (vf handleptr)
  (buffer (:pointer :char))
  (length :int)
  (bigendianp (:boolean :int))
  (word :int)
  (signed (:boolean :int))
  (bitstream (:pointer :int)))


;;; Seeking

(defcfun "ov_pcm_seek_lap" :int
  (vf handleptr)
  (position :int64))

(defcfun "ov_pcm_seek_page_lap" :int
  (vf handleptr)
  (position :int64))

(defcfun "ov_time_seek_lap" :int
  (vf handleptr)
  (seconds :double))

(defcfun "ov_time_seek_page_lap" :int
  (vf handleptr)
  (seconds :double))


;;; File information

(defcfun "ov_bitrate" :long
  (vf handleptr)
  (link :int))

(defcfun "ov_bitrate_instant" :long
  (vf handleptr))

(defcfun "ov_streams" :long
  (vf handleptr))

(defcfun "ov_seekable" :long
  (vf handleptr))

(defcfun "ov_serial_number" :long
  (vf handleptr)
  (link :int))

(defcfun "ov_pcm_total" :int64
  (vf handleptr)
  (link :int))

(defcfun "ov_time_total" :double
  (vf handleptr)
  (link :int))

(defcfun "ov_pcm_tell" :int64
  (vf handleptr))

(defcfun "ov_time_tell" :double
  (vf handleptr))

(defcfun "ov_info" (:pointer vorbis-info)
  (vf handleptr)
  (link :int))

(defcfun "ov_comment" (:pointer vorbis-comment)
  (vf handleptr)
  (link :int))


;;;; Cleaner interface

(defun open-oggvorbis-file (path)
  "Open an Ogg Vorbis file and return a pointer to the file handle."
  (let ((vf (foreign-alloc 'oggvorbis-file)))
    (when (zerop (ov-fopen path vf))
      vf)))

(defun close-oggvorbis-file (vf)
  "Close the file handle of an Ogg Vorbis file."
  (unless (null-pointer-p vf)
    (ov-clear vf)
    (foreign-free vf)))

;;;; Information

(defun get-vorbis-info-struct (vf &optional (link -1))
  "Return a vorbis-info structure."
  (mem-ref (ov-info vf link) 'vorbis-info))

(defun get-vorbis-channels (vf &optional (link -1))
  (foreign-slot-value (get-vorbis-info-struct vf link) 'vorbis-info 'channels))

(defun get-vorbis-rate (vf &optional (link -1))
  (foreign-slot-value (get-vorbis-info-struct vf link) 'vorbis-info 'rate))

;;;; Comments

(defun get-vorbis-comment-struct (vf &optional (link -1))
  "Return a vorbis-comment structure."
  (mem-ref (ov-comment vf link) 'vorbis-comment))

(defun get-vorbis-comments (vf &optional (link -1))
  "Returns a list of unparsed vorbis comment strings."
  (let* ((comment-struct (get-vorbis-comment-struct vf link))
         (len (foreign-slot-value comment-struct 'vorbis-comment 'comments))
         (comments (foreign-slot-value comment-struct 'vorbis-comment 'user-comments)))
    (loop for i upfrom 0 below len
          collect (mem-aref comments :string i))))

(defun get-vorbis-comment-from-comments (comments field)
  "From a list of unparsed vorbis comment strings, find the given field (a string)
  and return the value as a string, or NIL if it doesn't exist."
  (let* ((field-name (concatenate 'string field "="))
         (field-length (length field-name)))
    (loop for c in comments
          when (string-equal (subseq c 0 field-length) field-name)
          return (subseq c field-length))))

(defun get-vorbis-comment (vf field &optional (link -1))
  "From a given Ogg Vorbis file handle, retrieve the value of a given field
  from the comments section."
  (let ((comments (get-vorbis-comments vf link)))
    (get-vorbis-comment-from-comments comments field)))
