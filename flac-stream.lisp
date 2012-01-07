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
           #:flac-sample-rate)
  ;; ...
  (:import-from :flac #:buffer-position #:buffer-size #:stream-position #:num-channels #:buffer))

(in-package :mixalot-flac)

(defclass flac-streamer (flac-decoder)
  (
;;(handle      :reader flac-handle :initarg :handle)
   (sample-rate :reader flac-sample-rate :initarg :sample-rate)
   (output-rate :reader flac-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg :filename)
   (length    :initform nil)
   (seek-to   :initform nil)))

;;;; Callbacks

;;;; Helper functions

(defun make-flac-streamer
    (filename &rest args
     &key
     (output-rate 44100)
     (class 'flac-streamer)
     &allow-other-keys)
  (remf args :class)
  (let ((streamer (flac-open filename
                             :class class
                             :class-initargs (list* :filename filename
                                                    :output-rate output-rate
                                                    args)))
        initialized)
    (unwind-protect
         (flet ((prop (key) (getf (getf (flac-metadata streamer) :stream-info) key)))
           (unless (eql 16 (prop :bits-per-sample))
             (error 'flac-error :text "Expected 16-bit samples."))
           (unless (member (prop :channels) '(1 2))
             (error 'flac-error :text "Unsupported number of channels."))
           (unless (eql output-rate (prop :sample-rate))
             (warn "Sample-rate ~:D doesn't match output rate ~:D"
                   (prop :sample-rate) output-rate))
           (with-slots (sample-rate length) streamer
             (setf sample-rate (prop :sample-rate)
                   length (prop :total-samples)))
           (rotatef streamer initialized))
      (unless initialized
        (flac-close streamer)))
    initialized))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate buffer-position) stream
    (when seek-to
      (flac-seek stream seek-to)
      (setf seek-to nil))))

(defun buffer-consume (decoder n mix-buffer out-start)
  (with-slots (buffer buffer-position buffer-size num-channels stream-position) decoder
    (assert (and buffer buffer-size buffer-position))
    (assert (< buffer-position buffer-size))
    (assert (integerp num-channels))
    (flet ((update-position (index outidx)
             (setf buffer-position index)
             (incf stream-position (- outidx out-start))
             (when (= buffer-position buffer-size)
               (setf buffer-position nil
                     buffer-size nil))))
      (case num-channels
        (1 (loop for index from buffer-position below (min buffer-size (+ buffer-position n))
                 with outidx = out-start
                 do
                 (mixalot:stereo-mixf (aref mix-buffer outidx)
                                         (mixalot:mono->stereo (aref buffer index)))
                 (incf outidx)
                 finally
                 (update-position index outidx)
                 (return outidx)))
        (2 (loop for index from buffer-position below (min buffer-size (+ buffer-position (* 2 n))) by 2
                 with outidx = out-start
                 do
                 (mixalot:stereo-mixf (aref mix-buffer outidx)
                                      (mixalot:stereo-sample (aref buffer index)
                                                             (aref buffer (1+ index))))
                 (incf outidx)
                 finally
                 (update-position index outidx)
                 (return outidx)))))))

(defmethod streamer-mix-into ((streamer flac-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (type array-index offset length)
           (type sample-vector mix-buffer))
  (update-for-seek streamer)
  (with-slots (buffer buffer-position buffer-size num-channels position) streamer
    (loop with outidx = 0
          as remaining = (- length outidx)
          until (or (zerop remaining) (flac-eof streamer))
          do
          (assert (> remaining 0))
          (handler-case (unless buffer-position (flac::flac-refill-buffer streamer))
              (flac-error (err)
                ;; This seem to be ignorable.. =/
                (unless (eql :bad-header (flac-error-status err))
                  (mixer-remove-streamer mixer streamer)
                  (return-from streamer-mix-into))))
          (when buffer-position
            (setf outidx (buffer-consume streamer remaining mix-buffer outidx))))
    (when (flac-eof streamer)
      (mixer-remove-streamer mixer streamer))))

(defmethod streamer-cleanup ((stream flac-streamer) mixer)
  (declare (ignore mixer))
  (flac-close stream))

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
  (flac-stream-position stream))
