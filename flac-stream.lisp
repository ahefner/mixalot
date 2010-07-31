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

(defclass flac-streamer ()
  ((handle      :reader flac-handle :initarg :handle)
   (sample-rate :reader flac-sample-rate :initarg :sample-rate)
   (bits-per-sample :reader flac-bits-per-sample :initarg :bits-per-sample)
   (channels :reader flac-channels :initarg :channels)
   (output-rate :reader flac-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg filename)
   (buffer    :initform nil :accessor buffer)
   (buffer-position :initform 0 :accessor buffer-pos)
   (client-data :initform nil :initarg :client-data :accessor flac-client-data)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)))

;;;; Callbacks

(defcallback write-callback
             flac-decoder-write-status
             ((handle handleptr)
              (frame :pointer)
              (buffer :pointer)
              (client-data :pointer))
  (let* ((client-buffer (mem-ref client-data :pointer))
         (frame-header (foreign-slot-value frame 'flac::flac-frame 'flac::frame-header))
         (block-size (foreign-slot-value frame-header 'flac::flac-frame-header 'flac::block-size))
         (channels (foreign-slot-value frame-header 'flac::flac-frame-header 'flac::channels)))
    (setf (mem-aref client-buffer 'flac-int32 0) block-size)
    (loop for block from 0 below block-size
          do (loop for channel from 0 below channels
                   do (setf (mem-aref client-buffer 'flac-int16 (+ (* channels block) channel 2))
                            (mem-aref (mem-aref buffer :pointer channel) 'flac-int32 block))))
  :write-continue))


(defcallback metadata-callback
             :void
             ((handle handleptr)
              (metadata metadataptr)
              (client-data :pointer))
  (let ((type (foreign-slot-value metadata 'flac-metadata 'flac::type))
        (data (foreign-slot-pointer metadata 'flac-metadata 'flac::data)))
    (when (eql type :stream-info)
      (let ((stream-info (foreign-slot-pointer data 'flac-metadata-data 'flac::stream-info))
            (client-buffer (mem-ref client-data :pointer)))
        (with-foreign-slots ((flac::sample-rate flac::channels flac::bits-per-sample flac::total-samples) stream-info flac-metadata-stream-info)
          (setf (mem-aref client-buffer 'flac-uint64 0) flac::sample-rate
                (mem-aref client-buffer 'flac-uint64 1) flac::channels
                (mem-aref client-buffer 'flac-uint64 2) flac::bits-per-sample
                (mem-aref client-buffer 'flac-uint64 3) flac::total-samples)))))
  nil)

(defcallback error-callback
             :void
             ((handle handleptr)
              (status flac-decoder-error-status)
              (client-data :pointer))
  (error 'flac-error "Stream decoder error from callback" (flac-strerror status))
  nil)

(defun open-flac-file (filename &key (output-rate 44100))
  (let ((uhandle (flac-decoder-new))
        (client-data (foreign-alloc :pointer))
        (client-buffer (foreign-alloc 'flac-uint64 :count 4))
        handle)
    (setf (mem-aref client-data :pointer) client-buffer)
    (unwind-protect
      (progn
        (flac-decoder-init-file uhandle filename
                                (callback write-callback)
                                (callback metadata-callback)
                                (callback error-callback)
                                client-data)
        (flac-decoder-process-until-end-of-metadata uhandle)
        (unless (= (mem-aref client-buffer 'flac-uint64 1) 2)
          (error 'flac-error "Open FLAC file"
                 "Can only handle stereo FLAC streams."))
        (unless (= (mem-aref client-buffer 'flac-uint64 2) 16)
          (error 'flac-error "Open FLAC file"
                 "Can only handle FLAC streams with 16 bits per sample."))
        (rotatef handle uhandle))
      (when uhandle
        (flac-decoder-delete uhandle)
        (foreign-free client-buffer)
        (foreign-free client-data)))
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

(defmethod streamer-cleanup ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (flac-streamer-release-resources stream))

(defun make-flac-streamer
    (filename &rest args 
     &key 
     (output-rate 44100)
     (class 'flac-streamer)
     &allow-other-keys)
  (multiple-value-bind (handle client-data)
    (open-flac-file filename :output-rate output-rate)
    (remf args :class)
    (remf args :prescan)
    (let* ((client-buffer (mem-ref client-data :pointer))
           (stream (apply #'make-instance
                          class
                          :handle handle                         
                          :client-data client-data
                          :sample-rate (mem-aref client-buffer 'flac-uint64 0)
                          :channels (mem-aref client-buffer 'flac-uint64 1)
                          :output-rate output-rate
                          'filename filename
                          args)))
      (let ((result (mem-aref client-buffer 'flac-uint64 3)))
        (when (> result 0)
          (setf (slot-value stream 'length) result)))
      (foreign-free client-buffer)
      (setf (mem-ref client-data :pointer) (null-pointer))
      stream)))

(defun flac-read-samples (streamer samples)
  "A fake read function.
  
  Make sure that at most the given amount of samples is ready in the stream buffer.
  Return the amount of samples (at most the given amount)."

  (declare (optimize (speed 3))
           (type flac-streamer streamer))
  (with-slots (buffer-position buffer handle) streamer
    (declare (type sample-vector buffer))
    (when (= (- buffer-position 1) (aref buffer 0))
      (flac-decoder-process-single handle)
      (setf buffer-position 1))
    (min samples (- (aref buffer 0) (- buffer-position 1)))))

(defun flac-eof (streamer)
  "Return true if the flac streamer is at its end and there is nothing in the buffer any more."
  (declare (optimize (speed 3))
           (type flac-streamer streamer))
  (with-slots (buffer-position buffer handle) streamer
    (declare (type sample-vector buffer))
    (and (eql (flac-decoder-get-state handle) :end-of-stream)
         (= buffer-position (aref buffer 0)))))

(defmethod streamer-mix-into ((streamer flac-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (optimize (speed 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))
;  (update-for-seek streamer)
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
      (setf (mem-aref (flac-client-data streamer) :pointer) bufptr)
      (loop with end-output-index = (the array-index (+ offset length))
            with output-index = offset
            with chunk-size = 0
            with samples-read = 0
            while (< output-index end-output-index) do 

            (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                  samples-read (the array-index (flac-read-samples streamer chunk-size)))

            (when (flac-eof streamer) (loop-finish))

            (loop for out-idx upfrom (the array-index output-index)
                  for in-idx upfrom (the array-index (slot-value streamer 'buffer-position))
                  repeat samples-read
                  do (stereo-mixf (aref mix-buffer out-idx)
                                  (aref read-buffer in-idx)))
            (incf output-index samples-read)
            (incf (slot-value streamer 'position) samples-read)
            (incf (slot-value streamer 'buffer-position) samples-read)
            finally
            (when (flac-eof streamer)
              (mixer-remove-streamer mixer streamer))))))
