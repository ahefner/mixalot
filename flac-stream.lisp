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
  (:use :common-lisp :cffi :mixalot :flac)
  (:export #:flac-streamer
           #:make-flac-streamer
           #:flac-sample-rate
           #:flac-streamer-release-resources))

(in-package :mixalot-flac)

(defcstruct flac-streamer-client-data
  (client-buffer :pointer)
  (sample-rate flac::flac-unsigned) 
  (channels flac::flac-unsigned)
  (bits-per-sample flac::flac-unsigned) 
  (block-size flac::flac-unsigned) 
  (length flac::flac-uint64)) 

(defclass flac-streamer ()
  ((handle      :reader flac-handle :initarg :handle)
   (output-rate :reader flac-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg filename)
   (buffer    :initform nil :accessor buffer)
   (buffer-position :initform 0 :accessor buffer-pos)
   (client-data :initform nil :initarg :client-data :accessor flac-client-data)
   (position  :initform 0)
   (seek-to   :initform nil)))

(defun flac-sample-rate (streamer)
  (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'sample-rate)) 

(defun flac-bits-per-sample (streamer)
  (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'bits-per-sample)) 

(defun flac-channels (streamer)
  (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'channels)) 

(defun flac-length (streamer)
  (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'length)) 

(defun flac-block-size (streamer)
  (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'block-size)) 


;;;; Callbacks

(defcallback write-callback
             flac-decoder-write-status
             ((handle handleptr)
              (frame :pointer)
              (buffer :pointer)
              (client-data :pointer))
  (with-foreign-slots ((client-buffer block-size) client-data flac-streamer-client-data)
    (let* ((frame-header (foreign-slot-value frame 'flac::flac-frame 'flac::frame-header))
           (channels (foreign-slot-value frame-header 'flac::flac-frame-header 'flac::channels)))
      (setf block-size (foreign-slot-value frame-header 'flac::flac-frame-header 'flac::block-size))
      (loop for block from 0 below block-size
            do (loop for channel from 0 below channels
                     do (setf (mem-aref client-buffer 'flac-int16 (+ (* channels block) channel))
                              (mem-aref (mem-aref buffer :pointer channel) 'flac-int32 block))))
      :write-continue)))

(defcallback metadata-callback
             :void
             ((handle handleptr)
              (metadata metadataptr)
              (client-data :pointer))
  (let ((type (foreign-slot-value metadata 'flac-metadata 'flac::type))
        (data (foreign-slot-pointer metadata 'flac-metadata 'flac::data)))
    (when (eql type :stream-info)
      (let ((stream-info (foreign-slot-pointer data 'flac-metadata-data 'flac::stream-info)))
        (with-foreign-slots ((flac::sample-rate flac::channels flac::bits-per-sample flac::total-samples) stream-info flac-metadata-stream-info)
          (with-foreign-slots ((sample-rate channels bits-per-sample length) client-data flac-streamer-client-data)
            (setf sample-rate flac::sample-rate
                  channels flac::channels
                  bits-per-sample flac::bits-per-sample
                  length flac::total-samples))))))
  nil)

(defcallback error-callback
             :void
             ((handle handleptr)
              (status flac-decoder-error-status)
              (client-data :pointer))
  (error 'flac-error "Stream decoder error from callback" (flac-strerror status))
  nil)

;;;; Helper functions

(defun open-flac-file (filename &key (output-rate 44100))
  (let ((uhandle (flac-decoder-new))
        (client-data (foreign-alloc 'flac-streamer-client-data))
        rate
        handle)
    (unwind-protect
      (progn
        (flac-decoder-set-md5-checking uhandle 0)
        (flac-decoder-set-metadata-ignore-all uhandle)
        (flac-decoder-set-metadata-respond uhandle :stream-info)
        (flac-decoder-init-file uhandle filename
                                (callback write-callback)
                                (callback metadata-callback)
                                (callback error-callback)
                                client-data)
        (flac-decoder-process-until-end-of-metadata uhandle)
        (unless (= 16 (foreign-slot-value client-data 'flac-streamer-client-data 'bits-per-sample))
          (error 'flac-error "Open FLAC file" "Don't know how to handle non-16-bit samples"))
        (unless (= 2 (foreign-slot-value client-data 'flac-streamer-client-data 'channels))
          (error 'flac-error "Open FLAC file" "Don't know how to handle non-stereo streams"))
        (unless (= output-rate (setf rate (foreign-slot-value client-data 'flac-streamer-client-data 'sample-rate)))
          (warn "Sample rate doesn't match requested rate: ~:D vs expected ~:D" rate output-rate))
        (rotatef handle uhandle)
        (when uhandle
          (flac-decoder-delete uhandle)
          (foreign-free client-data))))
    (values handle client-data))) 

(defun flac-streamer-release-resources (flac-stream)
  "Release foreign resources associated with the flac-stream."
  (with-slots (handle client-data) flac-stream
    (when handle
      (flac-decoder-finish handle)
      (flac-decoder-delete handle)
      (setf handle nil))
    (when client-data
      (foreign-free client-data)
      (setf client-data nil))))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate) stream
    (when (and seek-to (flac-seek handle seek-to))
      (with-foreign-object (pos 'flac::flac-uint64)
        (flac-decoder-get-decode-position handle pos)
        (setf seek-to nil
              position (floor (* output-rate (mem-ref pos 'flac::flac-uint64)) sample-rate))))))

(defun flac-read-samples (streamer samples)
  "A fake read function.
  
  Make sure that at most the given amount of samples is ready in the stream buffer.
  Return the amount of samples (at most the given amount)."

  (declare (optimize (speed 3)))
  (with-slots (buffer-position buffer handle) streamer
    (declare (type sample-vector buffer)
             (type array-index buffer-position))
    (when (= buffer-position (flac-block-size streamer))
      (update-for-seek streamer)
      (flac-decoder-process-single handle)
      (setf buffer-position 0))
    (min samples (- (flac-block-size streamer) buffer-position))))

(defun flac-eof (streamer)
  "Return true if the flac streamer is at its end and there is nothing in the buffer any more."
  (declare (optimize (speed 3)))
  (with-slots (buffer-position buffer handle) streamer
    (declare (type sample-vector buffer)
             (type array-index buffer-position))
    (and (eql (flac-decoder-get-state handle) :end-of-stream)
         (= buffer-position (flac-block-size streamer)))))

(defun make-flac-streamer
    (filename &rest args 
     &key 
     (output-rate 44100)
     (class 'flac-streamer)
     &allow-other-keys)
  (multiple-value-bind (handle client-data)
    (open-flac-file filename :output-rate output-rate)
    (remf args :class)
    (let* ((stream (apply #'make-instance
                          class
                          :handle handle                         
                          :client-data client-data
                          :output-rate output-rate
                          'filename filename
                          args)))
      stream)))

(defmethod streamer-mix-into ((streamer flac-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (optimize (speed 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))
  (let* ((handle (flac-handle streamer))
         (channels (flac-channels streamer))
         (max-buffer-length 8192)
         (read-buffer (or (buffer streamer)
                          (setf (buffer streamer)
                                (make-array max-buffer-length
                                            :element-type 'stereo-sample
                                            :initial-element 0)))))
    (declare (type sample-vector read-buffer))
    (mixalot:with-array-pointer (bufptr read-buffer)
      (setf (foreign-slot-value (flac-client-data streamer) 'flac-streamer-client-data 'client-buffer) bufptr)
      (with-slots (buffer-position position) streamer
        (declare (type array-index buffer-position position))
        (loop with end-output-index = (the array-index (+ offset length))
              with output-index = offset
              with chunk-size = 0
              with samples-read = 0
              while (< output-index end-output-index) do

              (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                    samples-read (the array-index (flac-read-samples streamer chunk-size)))

              (when (flac-eof streamer) (loop-finish))

              (loop for out-idx upfrom (the array-index output-index)
                    for in-idx upfrom buffer-position
                    repeat samples-read
                    do (stereo-mixf (aref mix-buffer out-idx)
                                    (aref read-buffer in-idx)))
              (incf output-index samples-read)
              (incf position samples-read)
              (incf buffer-position samples-read)
              finally
              (when (flac-eof streamer)
                (mixer-remove-streamer mixer streamer)))))))

(defmethod streamer-cleanup ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (flac-streamer-release-resources stream))

;;; Seek protocol

;; I'm not sure how to determine if a FLAC stream is seekable without seeking.
;; For now, just assume it is seekable and quietly fail to seek if it isn't.
(defmethod streamer-seekable-p ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  t)

(defmethod streamer-length ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (flac-length stream))

(defmethod streamer-seek ((stream flac-streamer) mixer position 
                          &key &allow-other-keys)
  (declare (ignore mixer))
  (with-slots (seek-to sample-rate output-rate) stream
    (setf seek-to (floor (* sample-rate position) output-rate)))
  (values))

(defmethod streamer-position ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (position) stream
    position))
