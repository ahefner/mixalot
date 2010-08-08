;;;; CFFI bindings to libvorbisfile

;;;; Copyright (c) 2010 Andy Hefner, Sumant S.R. Oemrawsingh

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sellcopies of the Software, and to 
;;;; permit persons to whom the Software is furnished to do so, subject
;;;;  to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :vorbisfile)

(define-foreign-library libvorbisfile
  (:darwin (:or "libvorbisfile.dylib" "libvorbisfile.3.dylib"
                "/opt/local/lib/libvorbisfile.dylib"
                "/opt/local/lib/libvorbisfile.3.dylib"))
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
  (bigendianp :int)
  (word :int)
  (signed :int)
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

(defcfun "ov_seekable" (:boolean :long)
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

(define-condition vorbis-error ()
  ((text :initarg :text))
  (:documentation "An error from the vorbisfile library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

;;; Simulate a strerror type function with text mostly copied from the
;;; vorbisfile manual.
(defvar *vorbis-strerror* (list
  (cons OV_HOLE "There was an interruption in the data.")
  (cons OV_EREAD "A read from media returned an error.")
  (cons OV_EFAULT "Internal logic fault; indicates a bug or heap/stack corruption.")
  (cons OV_EINVAL "Invalid argument value; possibly called with an Ogg Vorbis handle that isn't open.")
  (cons OV_ENOTVORBIS "Bitstream does not contain any Vorbis data.")
  (cons OV_EBADHEADER "Invalid Vorbis bitstream header.")
  (cons OV_EVERSION "Vorbis version mismatch.")
  (cons OV_EBADLINK "An invalid stream section was supplied, or the requested link is corrupt.")
  (cons OV_ENOSEEK "Bitstream is not seekable.")
  (cons OV_EOF "Indicates stream is at end of file immediately after a seek (making crosslap impossible as there's no preceeding decode state to crosslap).")))

(defun vorbis-strerror (result)
  (cdr (assoc result *vorbis-strerror* :test #'eql)))

(defun raise-vorbis-error (circumstance message)
  "Raise an error for the vorbisfile library. Circumstance is a string
  that describes the circumstance under which this error was raised, and
  message is a string that (tries to) explain the error."
  (error 'vorbis-error
         :text (format nil "~A: ~A" circumstance message)))

(defun check-vorbis-error (circumstance result)
  "Check if an error has occured in the vorbisfile library calls."
  (when (< result 0)
    (raise-vorbis-error circumstance
                        (vorbis-strerror result))))

(defun check-vorbis-pointer-error (circumstance pointer)
  "Check if an error has occured in the vorbisfile library calls pertaining to a pointer."
  (if (null-pointer-p pointer)
    (raise-vorbis-error circumstance
                        "Operation performed on invalid physical or logical stream.")
    pointer))

;;;; Helper functions

;; Allocate and free foreign resources
(defun vorbis-new ()
  "Return a new Ogg Vorbis handle."
  (foreign-alloc 'vorbis-file))

(defun vorbis-delete (handle)
  "Delete an Ogg Vorbis handle."
  (unless (or (null handle) (null-pointer-p handle)) 
    (foreign-free handle)))

;; Open and close a vorbis file on the given handle
(defun vorbis-open (filename handle)
  "Open an Ogg Vorbis file and attach it to the given handle."
  (check-vorbis-error "Open Ogg Vorbis file" (ov-fopen filename handle)))

(defun vorbis-close (handle)
  "Close an Ogg Vorbis file by its handle."
  (check-vorbis-error "Close Ogg Vorbis file" (ov-clear handle)))

;; Seek to a specific position using lapping (less/no clicking), and when
;; accurate is nil, it seeks faster. 
(defun vorbis-seek (handle position &key (accurate t))
  "Seek (rougly) to the given position in samples."
  (check-vorbis-error "Seeking in stream"
    (if accurate
      (ov-pcm-seek-lap handle position)
      (ov-pcm-seek-page-lap handle position))))

;;;; Information

;; Information struct contains some parameters of the audio stream.
(defun get-vorbis-info (handle &optional (link -1))
  "Return vorbis-info of the specified handle and logical bitstream."
  (mem-ref (check-vorbis-pointer-error "Retrieving vorbis info" (ov-info handle link)) 'vorbis-info))

(defun get-vorbis-channels (handle &optional (link -1))
  "Return the number of channels in a Vorbis stream."
  (foreign-slot-value (get-vorbis-info handle link) 'vorbis-info 'channels))

(defun get-vorbis-rate (handle &optional (link -1))
  "Return the sample-rate in Hz."
  (foreign-slot-value (get-vorbis-info handle link) 'vorbis-info 'rate))

(defun get-vorbis-length (handle &optional (link -1))
  "Return the total number of samples in the physical stream. If link > 0,
  return the number of samples in that logical bitstream."
  (let ((err (ov-pcm-total handle link)))
    (check-vorbis-error "Retrieving total number of samples" err)
    err))

(defun get-vorbis-position (handle)
  "Return the current position in samples."
  (let ((err (ov-pcm-tell handle)))
    (check-vorbis-error "Retrieving current position" err)
    err))

;;;; Vorbis comments

(defun get-vorbis-comment (handle &optional (link -1))
  "Return a vorbis-comment."
  (mem-ref (ov-comment handle link) 'vorbis-comment))

(defun get-vorbis-raw-tags-from-comment (comment)
  "Returns all comments (without any processing) in a list. Most (all?) comments
  will be of the form KEY=VALUE, which can be considered as tags."
  (let ((length (foreign-slot-value comment 'vorbis-comment 'comments))
        (tags (foreign-slot-value comment 'vorbis-comment 'user-comments)))
    (loop for i upfrom 0 below length
          collect (mem-aref tags :string i))))

(defun get-vorbis-tag-value-from-raw-tags (tags tag-name)
  "Returns a tag's value from a list of raw tags."
  (let* ((tag-string (concatenate 'string tag-name "="))
         (tag-length (length tag-string))
         (tag-values
           (loop for comment in tags
                 when (and (< tag-length (length comment))
                           (equalp (subseq comment 0 tag-length) tag-string))
                 collect (subseq comment tag-length))))
    (when tag-values
      (format nil "~{~A~^, ~}" tag-values))))

;; This function contains some code from mpg123.lisp. I suggest the whole
;; tagging stuff is refactored.
(defun get-vorbis-tags-from-handle (handle &optional (link -1))
  "Returns a plist of tags with keys that are somewhat compatible with the MP3 ID3 tags."
  (let* ((comment (get-vorbis-comment handle link))
         (raw-tags (get-vorbis-raw-tags-from-comment comment)))
    (loop for (tag-name tag-keyword) in
          ;; The field names are the proposed standard names for vorbis
          ;; comments. I just map them onto the keys as used in mpg123.lisp
          ;; for ID3 tags as follows.
          '(("TITLE" :title)
            ("ARTIST" :artist)
            ("ALBUM" :album)
            ("DATE" :year) ; Post-processing should extract the year if a full date is given
            ("TRACKNUMBER" :track)
            ("GENRE" :genre)
            ("DESCRIPTION" :comment))
          as tag-value = (get-vorbis-tag-value-from-raw-tags raw-tags tag-name)
          when tag-value
          nconcing (list tag-keyword tag-value) into properties
          finally
          (setf (getf properties :track) (parse-integer (getf properties :track "0") :junk-allowed t))
          (return (clean-tags properties)))))

(defun get-vorbis-tags-from-file (filename &optional (link -1))
  "Open an Ogg Vorbis file, retrieve the tags, and close it."
  (with-foreign-object (handle 'vorbis-file)
     (vorbis-open filename handle)
     (unwind-protect
       (get-vorbis-tags-from-handle handle link)
       (vorbis-close handle))))
