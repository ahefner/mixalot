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
   (output-rate :reader flac-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg :filename)
   (buffer    :initform nil :accessor buffer)
   (channels :initform nil :reader flac-channels)
   (min-buffer-length :initform 0)
   (buffer-position :initform 0 :accessor buffer-pos)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)))

;;;; Callbacks

;;;; Helper functions

(defun open-flac-file (filename &key (output-rate 44100))
  (let* ((uhandle (flac-open filename))
         (stream-info (getf (flac-process-metadata uhandle) :stream-info))
         rate
         handle)
    (unwind-protect
      (progn
        (unless (= 16 (getf stream-info :bits-per-sample))
          (error 'flac-error "Open FLAC file" "Expected 16-bit samples."))
        (unless (= 2 (getf stream-info :channels))
          (error 'flac-error "Open FLAC file" "Expected stereo samples."))
        (unless (= output-rate (getf stream-info :sample-rate))
          (warn "Sample-rate doesn't match requested rate: ~:D vs expected ~:D"
                (getf stream-info :sample-rate) output-rate))
        (rotatef handle uhandle))
      (when uhandle
        (flac-close uhandle)))
    (values handle stream-info)))

(defun flac-streamer-release-resources (flac-stream)
  "Release foreign resources associated with the flac-stream."
  (with-slots (handle) flac-stream
    (when handle
      (flac-close handle)
      (setf handle nil))))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate buffer-position) stream
    (when (and seek-to (flac-seek handle seek-to))
      (let* ((client-data (flac-client-data handle))
             (block-size (flac-client-data-block-size client-data)))
        (setf seek-to nil
              buffer-position block-size ; fast-forward the buffer as well
              position (floor (* output-rate (flac-tell handle)) sample-rate))))))

(defun flac-streamer-read (stream samples)
  "Fake read function.

  Makes sure that at most the given amount of samples is ready in the streamer's buffer.
  The buffer must then be read starting from the streamer's buffer-position, and only
  for the length that is returned, even though more samples may be present."
  (declare (type array-index samples)
           (optimize (speed 3)))
  (with-slots (buffer buffer-position handle) stream
    (declare (type sample-vector buffer)
             (type array-index buffer-position))
    (let* ((client-data (flac-client-data handle))
           (block-size (flac-client-data-block-size client-data)))
      (declare (type array-index block-size))
      (when (= buffer-position block-size)
        (mixalot:with-array-pointer (bufptr buffer)
          (setf block-size (flac-read handle bufptr (length buffer))
                buffer-position 0)))
      (min samples (- block-size buffer-position)))))

(defun flac-streamer-eof (streamer)
  "Fake eof function.

  Checks if that the decoder is at the end of file, and that there's nothing in the
  streamer's buffer any more."
  (declare (optimize (speed 3)))
  (with-slots (buffer-position handle) streamer
    (declare (type array-index buffer-position))
    (let* ((client-data (flac-client-data handle))
           (block-size (flac-client-data-block-size client-data)))
      (declare (type array-index block-size))
      (and (flac-eof handle)
           (= buffer-position block-size)))))

(defun make-flac-streamer
    (filename &rest args 
     &key 
     (output-rate 44100)
     (class 'flac-streamer)
     &allow-other-keys)
  (multiple-value-bind (handle stream-info)
    (open-flac-file filename :output-rate output-rate)
    (remf args :class)
    (let* ((stream (apply #'make-instance
                          class
                          :handle handle                         
                          :output-rate output-rate
                          :filename filename
                          args)))
      (with-slots (min-buffer-length length sample-rate channels) stream
        (setf min-buffer-length (getf stream-info :maximum-block-size)
              sample-rate (getf stream-info :sample-rate)
              length (getf stream-info :total-samples)
              channels (getf stream-info :channels)))
      stream)))

(defmethod streamer-mix-into ((streamer flac-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (optimize (speed 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))
  (update-for-seek streamer)
  (let* ((channels (flac-channels streamer))
         (max-buffer-length (max 8192 (slot-value streamer 'min-buffer-length)))
         (read-buffer (or (buffer streamer)
                          (setf (buffer streamer)
                                (make-array max-buffer-length
                                            :element-type 'stereo-sample)))))
    (declare (type sample-vector read-buffer))
    (with-slots (buffer-position position) streamer
      (declare (type array-index buffer-position position max-buffer-length))
      (loop with end-output-index = (the array-index (+ offset length))
            with output-index = offset
            with chunk-size = 0
            with samples-read = 0
            while (< output-index end-output-index) do

            (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                  samples-read (the array-index (flac-streamer-read streamer chunk-size)))

            (when (flac-streamer-eof streamer) (loop-finish))

            (loop for out-idx upfrom (the array-index output-index)
                  for in-idx upfrom buffer-position
                  repeat samples-read
                  do (stereo-mixf (aref mix-buffer out-idx)
                                  (aref read-buffer in-idx)))
            (incf output-index samples-read)
            (incf position samples-read)
            (incf buffer-position samples-read)
            finally
            (when (flac-streamer-eof streamer)
              (mixer-remove-streamer mixer streamer))))))

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
  (with-slots (length) stream 
    length))

(defmethod streamer-seek ((stream flac-streamer) mixer position 
                          &key &allow-other-keys)
  (declare (ignore mixer))
  (let ((sample-rate (flac-sample-rate stream)))
    (with-slots (seek-to output-rate) stream
      (setf seek-to (floor (* sample-rate position) output-rate)))
    (values)))

(defmethod streamer-position ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (position) stream
    position))
