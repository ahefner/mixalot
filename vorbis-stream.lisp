(defpackage :mixalot-vorbis
  (:use :common-lisp :cffi :mixalot :vorbisfile)
  (:export #:vorbis-streamer
           #:make-vorbis-streamer
           #:vorbis-sample-rate
           #:vorbis-streamer-release-resources))

(in-package :mixalot-vorbis)

(defclass vorbis-streamer ()
  ((handle      :reader vorbis-handle :initarg :handle)
   (sample-rate :reader vorbis-sample-rate :initarg :sample-rate)
   (output-rate :reader vorbis-output-rate :initarg :output-rate)
   (filename  :initform nil :initarg filename)
   (buffer    :initform nil :accessor buffer)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)))

(defun open-vorbis-file (filename &key (output-rate 44100) (link 0))
  "Open an Ogg Vorbis file from disk and return the handle and sample
  rate of the given logical bitstream."
  (let* ((handle (oggvorbis-new))
         (result (oggvorbis-open filename handle))
        rate)
    (flet ((raise-error (msg)
             (oggvorbis-delete handle)
             (raise-oggvorbis-error "Open Ogg Vorbis file" msg)))
      (format nil "open: ~A~%" result)
      (if (zerop result)
        (if (= output-rate (setf rate (get-oggvorbis-rate handle link)))
          (values handle rate)
          (raise-error "Sample rate doesn't match requested rate."))
        (raise-error (oggvorbis-strerror result))))))

(defun vorbis-streamer-release-resources (vorbis-stream)
  "Release foreign resources associated with the vorbis-stream."
  (with-slots (handle) vorbis-stream
    (when handle
      (oggvorbis-close handle)
      (oggvorbis-delete handle)
      (setf handle nil))))

(defmethod streamer-cleanup ((stream vorbis-streamer) mixer)
  (declare (ignore mixer))
  (vorbis-streamer-release-resources stream))

(defun make-vorbis-streamer
    (filename &rest args 
     &key 
     (output-rate 44100)
     (class 'vorbis-streamer)
     (link 0)
     &allow-other-keys)
  "Create an ogg vorbis audio stream from a file, raising an oggvorbis-error
  if the file cannot be opened or another error occurs."
  (multiple-value-bind (handle sample-rate)
      (open-vorbis-file filename :output-rate output-rate :link link)
    (remf args :class)
    (remf args :link)
    (let ((stream (apply #'make-instance 
                         class
                         :handle handle                         
                         :sample-rate sample-rate
                         :output-rate output-rate
                         'filename filename
                         args)))
      (with-slots (length handle) stream
        (let ((result (get-oggvorbis-length handle link)))
          (when (> result 0)
            (setf length result))))
      stream)))

(defmethod streamer-mix-into ((streamer vorbis-streamer) mixer mix-buffer offset length time)
  (declare (ignore time)
           (optimize (speed 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))
  (with-foreign-object (bitstream :int)
    (let* ((max-buffer-length 8192)
           (handle (vorbis-handle streamer))
           (read-buffer (or (buffer streamer)
                            (setf (buffer streamer)
                                  (make-array max-buffer-length
                                              :element-type 'stereo-sample)))))
      (declare (type sample-vector read-buffer))
      (mixalot:with-array-pointer (bufptr read-buffer)
        (loop with end-output-index = (the array-index (+ offset length))
              with output-index = offset
              with nread = 0
              with samples-read = 0
              with chunk-size = 0
              while (< output-index end-output-index) do 

              (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                    nread (ov-read handle bufptr (* 4 chunk-size) 0 2 1 bitstream)
                    samples-read (the array-index (ash nread -2)))

              (when (< nread 0) (loop-finish))
              
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
                ((zerop nread) ; End of stream.
                 (mixer-remove-streamer mixer streamer))
                ((< nread 0)  ; Other error?
                 (format *trace-output* "~&~A (~A): error ~A: ~A~%" 
                         streamer 
                         (slot-value streamer 'filename)
                         nread
                         (oggvorbis-strerror nread ))
                 (mixer-remove-streamer mixer streamer))))))))
