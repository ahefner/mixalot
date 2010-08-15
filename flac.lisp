;;;; CFFI bindings to libFLAC

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

(in-package :flac)

(define-foreign-library libFLAC
  (:unix (:or "libFLAC.so.8"
              "/usr/lib/libFLAC.so"
              "/usr/local/lib/libFLAC.so"))
  (t (:default "libFLAC")))

(use-foreign-library libFLAC)

;;;; Interface to libFLAC (added as needed)

;;; Basic types

(defctype decoderptr :pointer)
(defctype metadataptr :pointer)

(defcvar ("FLAC__StreamDecoderErrorStatusString" +flac-stream-decoder-error-string+ :read-only t) :pointer)

;;; Error handling
(define-condition flac-error ()
  ((text :initarg :text))
  (:documentation "An error from the FLAC library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defun flac-strerror (status)
  (let ((string-pointer (get-var-pointer '+flac-stream-decoder-error-string+)))
    (mem-aref string-pointer :string status)))

;;; FFI

(defcfun ("FLAC__stream_decoder_new" flac-stream-decoder-new) decoderptr)

(defcfun ("FLAC__stream_decoder_delete" flac-stream-decoder-delete) :void
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_get_total_samples" flac-stream-decoder-get-total-samples) flac-uint64
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_get_channels" flac-stream-decoder-get-channels) flac-unsigned
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_init_file" flac-stream-decoder-init-file) flac-stream-decoder-init-status
  (decoder decoderptr)
  (filename :string)
  (write-callback :pointer)
  (metadata-callback :pointer)
  (error-callback :pointer)
  (client-data :pointer))

(defcfun ("FLAC__stream_decoder_finish" flac-stream-decoder-finish) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_process_until_end_of_metadata" flac-stream-decoder-process-until-end-of-metadata) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_process_single" flac-stream-decoder-process-single) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_get_state" flac-stream-decoder-get-state) flac-stream-decoder-state
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_set_md5_checking" flac-stream-decoder-set-md5-checking) flac-bool
  (decoder decoderptr)
  (value flac-bool))

(defcfun ("FLAC__stream_decoder_set_metadata_ignore_all" flac-stream-decoder-set-metadata-ignore-all) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_set_metadata_respond" flac-stream-decoder-set-metadata-respond) flac-bool
  (decoder decoderptr)
  (type flac-metadata-type))

(defcfun ("FLAC__stream_decoder_seek_absolute" flac-stream-decoder-seek-absolute) flac-bool
  (decoder decoderptr)
  (sample flac-uint64))

(defcfun ("FLAC__stream_decoder_flush" flac-stream-decoder-flush) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_reset" flac-stream-decoder-reset) flac-bool
  (decoder decoderptr))

(defcfun ("FLAC__stream_decoder_get_decode_position" flac-stream-decoder-get-decode-position) flac-bool
  (decoder decoderptr)
  (position (:pointer flac-uint64)))


(defcfun ("FLAC__metadata_object_clone" flac-metadata-object-clone) metadataptr
  (metadata metadataptr))

(defcfun ("FLAC__metadata_object_delete" flac-metadata-object-delete) :void
  (metadata metadataptr))

;;;; Lisp interface

(defclass flac-handle ()
  ((decoder-handle :reader flac-decoder-handle :initarg :decoder-handle)
   (client-data :reader flac-client-data :initarg :client-data)))

(defcstruct flac-client-data
  (metadata metadataptr)
  (buffer :pointer)
  (buffer-size flac-unsigned)
  (block-size flac-unsigned))

(defmacro flac-client-data-metadata (client-data)
  `(foreign-slot-value ,client-data 'flac-client-data 'metadata))

(defmacro flac-client-data-buffer (client-data)
  `(foreign-slot-value ,client-data 'flac-client-data 'buffer))

(defmacro flac-client-data-buffer-size (client-data)
  `(foreign-slot-value ,client-data 'flac-client-data 'buffer-size))

(defmacro flac-client-data-block-size (client-data)
  `(foreign-slot-value ,client-data 'flac-client-data 'block-size))

(defun flac-metadata-type (metadata)
  (foreign-slot-value metadata 'flac-metadata 'type))

;;;; Callbacks
(defcallback write-callback-old
             flac-stream-decoder-write-status
             ((handle decoderptr)
              (frame :pointer)
              (buffer :pointer)
              (client-data :pointer))
  :write-abort)

(defcallback write-callback
             flac-stream-decoder-write-status
             ((handle decoderptr)
              (frame :pointer)
              (decoder-buffer :pointer)
              (client-data :pointer))
  (declare (ignore handle))
  (with-foreign-slots ((buffer-size buffer block-size) client-data flac-client-data)
    (let* ((frame-header (foreign-slot-value frame 'flac-frame 'frame-header))
           (channels (foreign-slot-value frame-header 'flac-frame-header 'channels)))
      (setf block-size (foreign-slot-value frame-header 'flac-frame-header 'block-size))
      (when (< buffer-size block-size)
        (error 'flac-error "Write callback" "Buffer size smaller than block size"))
      (loop for block from 0 below block-size
            do (loop for channel from 0 below channels
                     do (setf (mem-aref buffer 'flac-int16 (+ (* channels block) channel))
                              (mem-aref (mem-aref decoder-buffer :pointer channel) 'flac-int32 block))))
      :write-continue)))

(defcallback error-callback
             :void
             ((handle decoderptr)
              (status flac-stream-decoder-error-status)
              (client-data :pointer))
  (declare (ignore handle client-data)
           (optimize (speed 3)))
  (error 'flac-error "Stream decoder error from callback" (flac-strerror status))
  nil)

(defcallback metadata-callback
             :void
             ((handle decoderptr)
              (metadata metadataptr)
              (client-data :pointer))
  (declare (ignore handle)
           (optimize (speed 3)))
  (setf (foreign-slot-value client-data 'flac-client-data 'metadata)
        (flac-metadata-object-clone metadata)))

;;; Opening and closing a file
(defun flac-open (filename &key (character-encoding :iso-8859-1)
                                (with-metadata '(:stream-info :vorbis-comment)))
  (let* ((dec-handle (flac-stream-decoder-new))
         (data (foreign-alloc 'flac-client-data))
         (uhandle (make-instance 'flac-handle
                                 :decoder-handle dec-handle
                                 :client-data data))
         handle)
    (unwind-protect
      (with-foreign-slots ((metadata buffer block-size) data flac-client-data)
        (setf metadata (null-pointer)
              buffer (null-pointer)
              block-size 0)
        (flac-stream-decoder-set-md5-checking dec-handle 0)
        (flac-stream-decoder-set-metadata-ignore-all dec-handle)
        (map nil (lambda (x) (flac-stream-decoder-set-metadata-respond dec-handle x))
             with-metadata)
        (with-foreign-string (unmangled filename :encoding character-encoding)
          (flac-stream-decoder-init-file dec-handle unmangled
                                         (callback write-callback)
                                         (callback metadata-callback)
                                         (callback error-callback)
                                         data))
        (rotatef handle uhandle))
      (when uhandle
        (flac-stream-decoder-delete dec-handle)
        (foreign-free data)))
      handle))

(defun flac-close (handle)
  (with-slots (decoder-handle client-data buffer) handle
    (when decoder-handle
      (flac-stream-decoder-finish decoder-handle)
      (flac-stream-decoder-delete decoder-handle)
      (foreign-free client-data)
      (setf decoder-handle nil
            client-data nil))))


;;; Processing metadata
(defun flac-process-stream-info (metadata)
  (let ((stream-info (foreign-slot-pointer
                       (foreign-slot-pointer metadata 'flac-metadata 'data)
                       'flac-metadata-data 'stream-info)))
    (with-foreign-slots ((minimum-block-size maximum-block-size sample-rate
                          channels bits-per-sample total-samples)
                         stream-info flac-metadata-stream-info)
      (list :minimum-block-size minimum-block-size
            :maximum-block-size maximum-block-size
            :sample-rate sample-rate
            :channels channels
            :bits-per-sample bits-per-sample
            :total-samples total-samples))))

(defun vorbis-comment-raw-tags (vorbis-comment)
  (with-foreign-slots ((num-comments comments) vorbis-comment flac-metadata-vorbis-comment)
    (loop for i from 0 below num-comments
          for comment-entry = (mem-aref comments 'flac-metadata-vorbis-comment-entry i)
          collect (magic-string-conversion
                    (foreign-slot-value comment-entry
                                          'flac-metadata-vorbis-comment-entry 'entry)))))

(defun flac-process-vorbis-comment (metadata)
  (let* ((vorbis-comment (foreign-slot-pointer
                           (foreign-slot-pointer metadata 'flac-metadata 'data)
                           'flac-metadata-data 'vorbis-comment)))
    (vorbis-comment-raw-tags vorbis-comment)))

(defun flac-process-stream-info-by-type (metadata)
  (let ((type (flac-metadata-type metadata)))
    (values
      (case type
        (:stream-info (flac-process-stream-info metadata))
        (:vorbis-comment (flac-process-vorbis-comment metadata)))
      type)))

(defun flac-process-metadata (handle)
  (with-slots (decoder-handle client-data) handle
    (unless (eql (flac-stream-decoder-get-state decoder-handle) :search-for-metadata)
      (flac-stream-decoder-reset decoder-handle))
    (flac-stream-decoder-set-md5-checking decoder-handle 0)
    (loop with metadata
          with plist

          do (progn
               (flac-stream-decoder-process-single decoder-handle)
               (setf metadata (flac-client-data-metadata client-data)))

          if (null-pointer-p metadata)
          do (setf plist nil)
          else
          do (progn
               (multiple-value-bind (content type)
                 (flac-process-stream-info-by-type metadata)
                 (setf plist (list type content)))
               (flac-metadata-object-delete metadata)
               (setf (flac-client-data-metadata client-data) (null-pointer)))

          when plist
          nconcing plist

          until (eql (flac-stream-decoder-get-state decoder-handle) :search-for-frame-sync))))

(defun flac-read (handle buffer buffer-size)
  "Read number of samples from the handle and store them in the C buffer of size buffer-size.
  The number of samples that are read is returned."
  (declare (optimize (speed 3)))
  (with-slots (decoder-handle client-data) handle
    (let ((state (flac-stream-decoder-get-state decoder-handle)))
      (when (or (eql state :search-for-metadata)
                (eql state :read-metadata))
        (flac-process-metadata handle)
        (setf state (flac-stream-decoder-get-state decoder-handle)))
      (if (or (eql state :read-frame)
              (eql state :search-for-frame-sync))
        (progn
          (setf (flac-client-data-buffer client-data) buffer
                (flac-client-data-buffer-size client-data) buffer-size)
          (flac-stream-decoder-process-single decoder-handle)
          (flac-client-data-block-size client-data))
        0))))

(defun flac-seek (handle sample)
  (let ((decoder-handle (flac-decoder-handle handle)))
    (if (zerop (flac-stream-decoder-seek-absolute decoder-handle))
      (when (eql (flac-stream-decoder-get-state decoder-handle) :seek-error)
        (flac-stream-decoder-flush decoder-handle)
        nil)
      t)))

(defun flac-tell (handle)
  (with-foreign-object (pos 'flac-uint64)
    (flac-stream-decoder-get-position (flac-decoder-handle handle) pos)
    (mem-ref pos 'flac-uint64)))

(defun flac-eof (handle)
  (eql (flac-stream-decoder-get-state (flac-decoder-handle handle)) :end-of-stream))

(defun get-flac-tags-from-handle (handle)
  "Returns a plist of tags with keys that are somewhat compatible with the MP3 ID3 tags."
  (let* ((plist (flac-process-metadata handle))
         (comments (getf plist :vorbis-comment)))
    (vorbis-comments-to-tags comments)))

(defun get-flac-tags-from-file (filename &key (character-encoding :iso-8859-1))
  "Open a FLAC file, retrieve the tags, and close it."
  (let* ((handle (flac-open filename
                            :character-encoding character-encoding
                            :with-metadata '(:vorbis-comment)))
         (tags (get-flac-tags-from-handle handle)))
    (flac-close handle)
    tags))
