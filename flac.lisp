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
  ((text :initarg :text)
   (status :initarg :status :reader flac-error-status :initform nil))
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

(defclass flac-decoder ()
  ((decoder-handle :reader flac-decoder-handle :initarg :decoder-handle)
   (deferred-condition :accessor deferred-condition :initform nil)
   (num-channels :accessor flac-num-channels :initarg :num-channels)
   (raw-metadata :accessor flac-raw-metadata :initform nil)
   (metadata :accessor flac-metadata :initform nil)
   (stream-position :reader flac-stream-position  :initform 0)
   (buffer :accessor flac-decoder-buffer :initform nil)
   (buffer-position :accessor flac-buffer-position :initform nil)
   (buffer-size :accessor flac-buffer-size :initform nil)))

(defvar *flac-decoder* nil
  "Bound within FLAC functions to communicate lisp data to callbacks.")

;;;; Callbacks

;; We can't do non-local exits from a callback, so handle errors by
;; stashing the condition object in the decoder, and signalling it
;; when we've returned from C.
(defun defer-error (type &rest args)
  (setf (deferred-condition *flac-decoder*)
        (apply #'make-condition type args)))

(defun signal-deferred (decoder)
  (when (deferred-condition decoder)
    (error (shiftf (deferred-condition decoder) nil))))

;; A call to flac-stream-decoder-process-single pumps the decoder for
;; a chunk of data, which is delivered via this callback:
(defcallback write-callback
    flac-stream-decoder-write-status
    ((handle decoderptr)
     (frame :pointer)
     (decoder-buffer :pointer)
     (client-data :pointer))
  (declare (ignore handle client-data))
  (with-slots (buffer buffer-position buffer-size) *flac-decoder*
    (assert (null buffer-position))
    (let* ((frame-header (foreign-slot-value frame 'flac-frame 'frame-header))
           (num-channels (foreign-slot-value frame-header 'flac-frame-header 'channels))
           (block-size (foreign-slot-value frame-header 'flac-frame-header 'block-size))
           (num-samples (* num-channels block-size)))
      ;; Ensure sufficient buffer size:
      (when (< (length buffer) num-samples)
        (setf buffer (make-array num-samples :element-type '(signed-byte 16) :fill-pointer nil :adjustable nil)))
      (setf buffer-size num-samples
            buffer-position 0)
      ;; Copy channels to output
     (setf (slot-value *flac-decoder* 'num-channels) num-channels) ; Hrm.
     (dotimes (channel num-channels)
       (loop for in-index from 0 below block-size
             for out-index upfrom channel by num-channels
             do (setf (aref buffer out-index)
                      (mem-aref (mem-aref decoder-buffer :pointer channel) 'flac-int32 in-index))))
     :write-continue)))

(defcallback error-callback
             :void
             ((handle decoderptr)
              (status flac-stream-decoder-error-status)
              (client-data :pointer))
  (declare (ignore handle client-data))
  (unless (eql :lost-sync status)
    (defer-error 'flac-error
        :text (format nil "FLAC decoder error from callback: ~A" status)
        :status status))
  nil)

(defcallback metadata-callback
             :void
             ((handle decoderptr)
              (mptr metadataptr)
              (client-data :pointer))
  (declare (ignore handle client-data))
  (push (flac-metadata-object-clone mptr)
        (flac-raw-metadata *flac-decoder*)))

;;; Opening and closing a file

(defun free-raw-metadata (decoder)
  (with-slots (raw-metadata) decoder
    (map nil 'flac-metadata-object-delete (flac-raw-metadata decoder))
    (setf raw-metadata nil)))

(defun flac-open (filename &key
                  (character-encoding :iso-8859-1)
                  (class 'flac-decoder)
                  class-initargs)
  (let* ((decoder-handle (flac-stream-decoder-new))
         (*flac-decoder* (apply #'make-instance
                                class
                                :decoder-handle decoder-handle
                                class-initargs))
         initialized-decoder)
    (unwind-protect
         (progn
           (flac-stream-decoder-set-md5-checking decoder-handle 0)
           (flac-stream-decoder-set-metadata-ignore-all decoder-handle)
           (flac-stream-decoder-set-metadata-respond decoder-handle :stream-info)
           (flac-stream-decoder-set-metadata-respond decoder-handle :vorbis-comment)
           (with-foreign-string (unmangled filename :encoding character-encoding)
             (flac-stream-decoder-init-file decoder-handle unmangled
                                            (callback write-callback)
                                            (callback metadata-callback)
                                            (callback error-callback)
                                            (null-pointer)))
           (setf (flac-metadata *flac-decoder*) (flac-process-metadata *flac-decoder*))
           (setf initialized-decoder *flac-decoder*))

      (unless initialized-decoder
        (flac-stream-decoder-delete decoder-handle)
        (free-raw-metadata *flac-decoder*)))

    initialized-decoder))

(defun flac-close (decoder)
  (with-slots (decoder-handle buffer metadata) decoder
    (setf buffer nil)
    (free-raw-metadata decoder)
    (when decoder-handle
      (flac-stream-decoder-finish decoder-handle)
      (flac-stream-decoder-delete decoder-handle)
      (setf decoder-handle nil))))

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

(defun flac-process-vorbis-comment (metadata)
  (let* ((vorbis-comment (foreign-slot-pointer
                           (foreign-slot-pointer metadata 'flac-metadata 'data)
                           'flac-metadata-data 'vorbis-comment)))
    (with-foreign-slots ((num-comments comments) vorbis-comment flac-metadata-vorbis-comment)
      (loop for i from 0 below num-comments
            for comment-entry = (mem-aref comments 'flac-metadata-vorbis-comment-entry i)
            collect (magic-string-conversion
                      (foreign-slot-value comment-entry
                                          'flac-metadata-vorbis-comment-entry 'entry))))))

(defun flac-metadata-type (metadata)
  (foreign-slot-value metadata 'flac-metadata 'type))

(defun flac-process-metadata-block (metadata)
  (let ((type (flac-metadata-type metadata)))
    (list
      type
      (case type
        (:stream-info (flac-process-stream-info metadata))
        (:vorbis-comment (flac-process-vorbis-comment metadata))))))

(defun flac-process-metadata (decoder)
  (let ((*flac-decoder* decoder))
   (with-slots (decoder-handle) decoder
     ;; Reset stream. Dubious. Discuss.
     (unless (eql (flac-stream-decoder-get-state decoder-handle) :search-for-metadata)
       (flac-stream-decoder-reset decoder-handle))
     (flac-stream-decoder-set-md5-checking decoder-handle 0)
     ;; Scan through metadata blocks.
     (flac-stream-decoder-process-until-end-of-metadata decoder-handle)
     (signal-deferred decoder)
     ;; Interpret metadata blocks.
     (mapcan #'flac-process-metadata-block (flac-raw-metadata decoder)))))

(defun flac-refill-buffer (decoder)
  (let ((*flac-decoder* decoder))
    (flac-stream-decoder-process-single (flac-decoder-handle decoder))
    (flac-buffer-size decoder)
    (signal-deferred decoder)))

(defun flac-seek (decoder sample)
  (let ((*flac-decoder* decoder))
   (with-slots (decoder-handle buffer-size buffer-position stream-position) decoder
     (setf buffer-size nil
           buffer-position nil)
     ;; WTF. The return values here don't seem to correlate with
     ;; whether the seek succeeded or not. I don't like doing this,
     ;; but I'm just going to set the stream position as though it
     ;; always succeeds:
     (setf stream-position sample)
     (cond
       ;; Seek failed:
       ((zerop (flac-stream-decoder-seek-absolute decoder-handle sample))
        (signal-deferred decoder)
        (when (eql (flac-stream-decoder-get-state decoder-handle) :seek-error)
          (flac-stream-decoder-flush decoder-handle)))
       ;; Success?
       (t #+NIL (setf stream-position sample)))))
  (signal-deferred decoder))

(defun flac-eof (decoder)
  (with-slots (buffer-size buffer-position) decoder
    (and (eql buffer-size buffer-position)
         (case (flac-stream-decoder-get-state (flac-decoder-handle decoder))
             ((:end-of-stream :ogg-error) t) ; Not sure about errors.
             (otherwise nil)))))

(defun get-flac-tags-from-file (filename &key (character-encoding :iso-8859-1))
  "Open a FLAC file, retrieve the tags, and close it."
  (let ((decoder (flac-open filename
                            :character-encoding character-encoding)))
    (unwind-protect
         (vorbis-comments-to-tags
          (getf (flac-metadata decoder) :vorbis-comment))
      (flac-close decoder))))
