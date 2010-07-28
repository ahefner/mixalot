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

(defctype handleptr :pointer)
(defctype metadataptr :pointer)

(defcvar ("FLAC__StreamDecoderErrorStatusString" +flac-decoder-error-string+ :read-only t) (:pointer :string))

;;; Error handling
(define-condition flac-error ()
  ((text :initarg :text))
  (:documentation "An error from the FLAC library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defun flac-strerror (status)
  (mem-aref +flac-decoder-error-string+ :string status))

;;; FFI

(defcfun ("FLAC__stream_decoder_new" flac-decoder-new) handleptr)

(defcfun ("FLAC__stream_decoder_delete" flac-decoder-delete) :void
  (decoder handleptr))

(defcfun ("FLAC__stream_decoder_get_total_samples" flac-decoder-get-total-samples) flac-uint64
  (decoder handleptr))

(defcfun ("FLAC__stream_decoder_get_channels" flac-decoder-get-channels) flac-unsigned
  (decoder handleptr))

(defcfun ("FLAC__stream_decoder_init_file" flac-decoder-init-file) flac-decoder-init-status
  (decoder handleptr)
  (filename :string)
  (write-callback :pointer)
  (metadata-callback :pointer)
  (error-callback :pointer)
  (client-data :pointer))

(defcfun ("FLAC__stream_decoder_finish" flac-decoder-finish) flac-bool
  (decoder handleptr))

(defcfun ("FLAC__stream_decoder_process_until_end_of_metadata" flac-decoder-process-until-end-of-metadata) flac-bool
  (decoder handleptr))

;;;; Helper functions
