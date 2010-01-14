;;;; Mixalot MP3 streaming

;;;; Copyright (c) 2009 Andy Hefner

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

(defpackage :mixalot-mp3
  (:use :common-lisp :cffi :mixalot :mpg123)
  (:export #:mp3-streamer
           #:make-mp3-streamer
           #:mp3-sample-rate
           #:mp3-streamer-release-resources))

(in-package :mixalot-mp3)

(defclass mp3-streamer ()
  ((handle      :reader mpg123-handle :initarg :handle)
   (sample-rate :reader mp3-sample-rate :initarg :sample-rate)
   (output-rate :reader mp3-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg filename)
   (buffer    :initform nil :accessor buffer)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)))

(defun open-mp3-file (filename &key (output-rate 44100))
  "Open an MP3 file from disk, forcing the output format to 16 bit,
stereo, resampling to the requested rate, and returning an
mpg123_handle pointer if successful."
  (ensure-libmpg123-initialized)
  (let (handle uhandle rate)
    (unwind-protect
         (with-foreign-object (err :int)
           (setf uhandle (mpg123-new (null-pointer) err))
           (check-mpg123-plain-error "mpg123-new" (mem-ref err :int))
           (mpg123-param uhandle :add-flags MPG123_QUIET 0.0d0)
           (check-mh-error "Clear default formats" uhandle (mpg123-format-none uhandle))
           (check-mh-error "Configure output format" uhandle
             (mpg123-format uhandle output-rate 2 MPG123_ENC_SIGNED_16))
           (mpg123-param uhandle :force-rate output-rate 0.0d0)
           (with-foreign-string (name filename :encoding :iso-8859-1)
             (check-mh-error "Open mp3 file" uhandle (mpg123-open uhandle name)))

           ;; The library wants see the stream format before we begin decoding.
           (setf rate (mpg123-getformat uhandle))

           (check-mh-error "Prescan stream" uhandle (mpg123-scan uhandle))
           (rotatef handle uhandle))
      (when uhandle (mpg123-close uhandle)))
    (values handle rate)))

(defun mp3-streamer-release-resources (mp3-stream)
  "Release foreign resources associated with the mp3-stream."
  (with-slots (handle) mp3-stream
    (when handle
      (mpg123-close handle)
      (mpg123-delete handle)
      (setf handle nil))))

(defmethod streamer-cleanup ((stream mp3-streamer) mixer)
  (declare (ignore mixer))
  (mp3-streamer-release-resources stream))

(defun make-mp3-streamer
    (filename &rest args &key (output-rate 44100) (class 'mp3-streamer) &allow-other-keys)
  "Create an mp3 audio stream from a file, raising an mpg123-error if 
the file cannot be opened or another error occurs."
  (multiple-value-bind (handle sample-rate)
      (open-mp3-file filename :output-rate output-rate)
    (remf args :class)
    (let ((stream (apply #'make-instance 
                         class 
                         :handle handle
                         :sample-rate sample-rate
                         :output-rate output-rate
                         'filename filename
                         args)))
      (with-slots (length handle) stream
        (let ((result (mpg123-length handle)))
          (when (> result 0)
            (setf length result))))
      stream)))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate) stream
    (when seek-to 
      (mpg123-seek handle seek-to :set)
     (setf seek-to nil
           position (floor (* output-rate (mpg123-tell handle))
                           sample-rate)))))

(defmethod streamer-mix-into ((streamer mp3-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (optimize (speed 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))
  (update-for-seek streamer)
  (with-foreign-object (nread 'mpg123::size_t)
    (let* ((max-buffer-length  8192)
           (handle (mpg123-handle streamer))
           (read-buffer (or (buffer streamer)
                            (setf (buffer streamer)
                                  (make-array max-buffer-length
                                              :element-type 'stereo-sample)))))
      (declare (type sample-vector read-buffer))
      (mixalot:with-array-pointer (bufptr read-buffer)
        (loop with end-output-index = (the array-index (+ offset length))
              with output-index = offset
              with err = 0
              with samples-read = 0
              with chunk-size = 0
              while (< output-index end-output-index) do 

              (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                    err (mpg123-read handle bufptr (* 4 chunk-size) nread)
                    samples-read (the array-index (ash (mem-ref nread 'mpg123::size_t) -2)))

              (when (not (zerop err)) (loop-finish))
              
              ;; Mix into buffer
              (loop for out-idx upfrom (the array-index output-index)
                    for in-idx upfrom 0
                    repeat samples-read
                    do (stereo-mixf (aref mix-buffer out-idx) 
                                    (aref read-buffer in-idx)))
              (incf output-index samples-read)
              (incf (slot-value streamer 'position) samples-read)

              finally
              (cond
                ((= err MPG123_DONE) ; End of stream.
                 (mixer-remove-streamer mixer streamer))
                ((/= err 0)  ; Other error?
                 (format *trace-output* "~&~A (~A): error ~A: ~A~%" 
                         streamer 
                         (slot-value streamer 'filename)
                         err
                         (mpg123-strerror handle))
                 (mixer-remove-streamer mixer streamer))))))))

;;; Seek protocol

(defmethod streamer-seekable-p ((stream mp3-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (length stream) stream
    (not (not length))))

(defmethod streamer-length ((stream mp3-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (length) stream 
    length))

(defmethod streamer-seek ((stream mp3-streamer) mixer position 
                          &key &allow-other-keys)
  (declare (ignore mixer))
  (with-slots (seek-to sample-rate output-rate) stream
    (setf seek-to (floor (* sample-rate position) output-rate)))
  (values))

(defmethod streamer-position ((stream mp3-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (position) stream
    position))
