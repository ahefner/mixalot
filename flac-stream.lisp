;;;; Mixalot FLAC streaming

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

(defpackage :mixalot-flac
  (:use :common-lisp :cffi :mixalot :flac))

(in-package :mixalot-flac)

(defclass flac-streamer ()
  ((handle      :reader flac-handle :initarg :handle)
   (sample-rate :reader flac-sample-rate :initarg :sample-rate)
   (bits-per-sample :reader flac-bits-per-sample :initarg :bits-per-sample)
   (channels :reader flac-channels :initarg :channels)
   (output-rate :reader flac-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg filename)
   (buffer    :initform nil :accessor buffer)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)))

;;;; Callbacks

(defcallback write-callback
             flac-decoder-write-status
             ((handle handleptr)
              (frame :pointer)
              (buffer (:pointer (:pointer flac-int32)))
              (client-data :pointer))
  nil)

(defcallback metadata-callback
             :void
             ((handle handleptr)
              (metadata metadataptr)
              (client-data :pointer))
  (let ((type (foreign-slot-value metadata 'flac-metadata 'type))
        (data (foreign-slot-pointer metadata 'flac-metadata 'data)))
    (when (equal type :stream-info)
      (let ((stream-info (foreign-slot-pointer data 'flac-metadata-data 'stream-info)))
        (with-foreign-slots ((sample-rate channels bits-per-sample) stream-info flac-metadata-stream-info)
          (format t "Sample rate  : ~D Hz~%" sample-rate)
          (format t "Channels     : ~D~%" channels)
          (format t "Total samples: ~D~%" bits-per-sample))))))

(defcallback error-callback
             :void
             ((handle handleptr)
              (status flac-decoder-error-status)
              (client-data :pointer))
  (error 'flac-error "Stream decoder error from callback" (flac-strerror status)))

(defun flac-open (filename handle)
  (flac-decoder-init-file handle filename
                          (callback write-callback)
                          (callback metadata-callback)
                          (callback error-callback)
                          (null-pointer)))

(defun flac-streamer-release-resources (flac-stream)
  "Release foreign resources associated with the flac-stream."
  (with-slots (handle) flac-stream
    (when handle
      (flac-decoder-finish handle)
      (flac-decoder-delete handle)
      (setf handle nil))))

(defmethod streamer-cleanup ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (flac-streamer-release-resources stream))
