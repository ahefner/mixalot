(in-package :vorbisfile)

(define-foreign-library libvorbisfile
  (:unix (:or "libvorbisfile.so.3"
              "/usr/lib/libvorbisfile.so"
              "/usr/local/lib/libvorbisfile.so"))
  (t (:default "libvorbisfile")))

(use-foreign-library libvorbisfile)

;;;; Interface to libvorbisfile (added as needed)

;;; Basic types

(defctype handleptr :pointer)

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


;;;; Error handling

(define-condition oggvorbis-error ()
  ((text :initarg :text))
  (:documentation "An error from the vorbisfile library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

; This should maybe be an alist
(defvar *oggvorbis-strerror* nil)
(setf *oggvorbis-strerror* (acons OV_HOLE "There was an interruption in the data." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EREAD "A read from media returned an error." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EFAULT "Internal logic fault." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EINVAL "The initial file headers couldn't be read or are corrupt, or the initial open call failed." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_ENOTVORBIS "Bitstream does not contain any Vorbis data." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EBADHEADER "Invalid Vorbis bitstream header." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EVERSION "Vorbis version mismatch." *oggvorbis-strerror*))
(setf *oggvorbis-strerror* (acons OV_EBADLINK "An invalid stream section was supplied, or the requested link is corrupt." *oggvorbis-strerror*))

(defun oggvorbis-strerror (result)
  (cdr (assoc result *oggvorbis-strerror* :test #'eql)))

(defun raise-oggvorbis-error (circumstance message)
  "Raise an error for the vorbisfile library. Circumstance is a string
  that describes the circumstance under which this error was raised, and
  message is a string that (tries to) explain the error."
  (error 'oggvorbis-error
         :text (format nil "~A: ~A" circumstance message)))

(defun check-oggvorbis-error (circumstance result)
  (if (< result 0)
    (raise-oggvorbis-error circumstance
                           (oggvorbis-strerror result))
    result))

(defun check-oggvorbis-pointer-error (circumstance pointer)
  (if (null-pointer-p pointer)
    (raise-oggvorbis-error circumstance
                           "Operation performed on invalid physical or logical stream.")
    pointer))

;;;; Helper functions

(defun oggvorbis-new ()
  "Return a new Ogg Vorbis handle."
  (foreign-alloc 'oggvorbis-file))

(defun oggvorbis-delete (handle)
  "Delete an Ogg Vorbis handle."
  (unless (null-pointer-p handle)
    (foreign-free handle)))

(defun oggvorbis-open (filename handle)
  "Open an Ogg Vorbis file and attach the given handle."
  (check-oggvorbis-error "Open Ogg Vorbis file" (ov-fopen filename handle)))

(defun oggvorbis-close (handle)
  "Close an Ogg Vorbis file by its handle."
  (check-oggvorbis-error "Close Ogg Vorbis file" (ov-clear handle)))

;;;; Information

(defun get-vorbis-info (handle &optional (link -1))
  "Return vorbis-info of the specified handle and logical bitstream."
  (mem-ref (check-oggvorbis-pointer-error "Retrieving vorbis info" (ov-info handle link)) 'vorbis-info))

(defun get-oggvorbis-channels (handle &optional (link -1))
  "Return the number of channels in a Vorbis stream."
  (foreign-slot-value (get-vorbis-info handle link) 'vorbis-info 'channels))

(defun get-oggvorbis-rate (handle &optional (link -1))
  (foreign-slot-value (get-vorbis-info handle link) 'vorbis-info 'rate))

(defun get-oggvorbis-length (handle &optional (link -1))
  "Return the total number of samples in the physical stream. If link > 0,
  return the number of samples in that logical bitstream."
  (check-oggvorbis-error "Retrieving number of samples"
                         (ov-pcm-total handle link)))

;;;; Vorbis comments

;(defun add-tag-to-plist (field value plist)

(defun get-vorbis-comment (vf &optional (link -1))
  "Return a vorbis-comment."
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

(defun get-vorbis-tag (vf field &optional (link -1) (multi t))
  "From a given Ogg Vorbis file handle, retrieve the value of a given tag
  from the comments section."
  (let ((comments (get-vorbis-comments vf link)))
    (get-vorbis-comment-from-comments comments field)))

;;;; Helper functions

#|
(defun decode-oggvorbis-file (filename)
  (with-foreign-object (vf 'oggvorbis-file)
    (check-oggvorbis-error "Open Ogg Vorbis file" (ov-fopen filename vf))
    (unwind-protect
      (let* ((info (get-vorbis-info vf 0))
             (rate (foreign-slot-value info 'vorbis-info 'rate))
             (channels (foreign-slot-value info 'vorbis-info 'channels))
             (length (* channels
                        (check-oggvorbis-error
                          "Retrieving number of channels"
                          (ov-pcm-total vf 0))))
             (buffer (make-array length :element-type '(signed-byte 16))))
        (with-foreign-objects ((cbuffer :short 16384)
                               (bitstream :int))
          (loop as len = (ov-read vf cbuffer 32768 0 2 1 bitstream)
                as num-samples = (ash len -1)
                with idx = 0
                while (and (> len 0) (< idx length)) do
                (loop for outidx from idx below (min length (+ idx num-samples))
                      for index from 0 below num-samples do
                      (setf (aref buffer outidx)
                            (mem-aref cbuffer :short index)))
                (incf idx num-samples)))
        (return-from decode-oggvorbis-file
          (values buffer rate channels)))
      (check-oggvorbis-error "Close Ogg Vorbis file" (ov-clear vf)))))
|#
