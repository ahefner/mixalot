
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
   (channels  :initform 0 :accessor channels :initarg :channels)
   (length    :initform nil)
   (position  :initform 0)
   (seek-to   :initform nil)
   (io-buffer      :initform nil :accessor io-buffer)
   (convert-buffer :initform nil :accessor convert-buffer)))

;; Hmm, looks ugly...
(defun open-vorbis-file (filename &key (output-rate 44100) (link 0))
  "Open an Ogg Vorbis file from disk and return the handle and sample
  rate of the given logical bitstream."
  (let (handle uhandle rate channels)
    (unwind-protect
      (progn
        (setf uhandle (vorbis-new))
        (vorbis-open filename uhandle)
        (unwind-protect
          (progn
            (setf rate (get-vorbis-rate uhandle :link link)
                  channels (get-vorbis-channels uhandle :link link))
            ;; XXX DUBIOUS
            (unless (= output-rate rate)
              #+NIL
              (raise-vorbis-error "Open Ogg Vorbis file"
                                  "Sample rate doesn't match requested rate.")
              (warn "Sample rate doesn't match requested rate: ~:D vs expected ~:D"
                    rate output-rate))
            (unless (or (= channels 2) (= channels 1))
              (raise-vorbis-error "Open Ogg Vorbis file"
                                  "Vorbis file is not mono or stereo."))
            (rotatef handle uhandle))
          (when uhandle (vorbis-close uhandle))))
      (when uhandle (vorbis-delete uhandle)))
    (values handle rate channels)))

(defun vorbis-streamer-release-resources (vorbis-stream)
  "Release foreign resources associated with the vorbis-stream."
  (with-slots (handle) vorbis-stream
    (when handle
      (vorbis-close handle)
      (vorbis-delete handle)
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
  "Create an ogg vorbis audio stream from a file, raising an vorbis-error
  if the file cannot be opened or another error occurs."
  (multiple-value-bind (handle sample-rate channels)
      (open-vorbis-file filename :output-rate output-rate :link link)
    (remf args :class)
    (remf args :link)
    (let ((stream (apply #'make-instance 
                         class
                         :handle handle                         
                         :sample-rate sample-rate
                         :output-rate output-rate
                         :channels channels
                         'filename filename
                         args)))
      (with-slots (length handle) stream
        (let ((result (get-vorbis-length handle :link link)))
          (when (> result 0)
            (setf length result))))
      stream)))

(defun update-for-seek (stream)
  (with-slots (handle seek-to position output-rate sample-rate) stream
    (when seek-to 
      (vorbis-seek handle seek-to)
     (setf seek-to nil
           position (floor (* output-rate (get-vorbis-position handle))
                           sample-rate)))))

(defmethod streamer-mix-into ((streamer vorbis-streamer) mixer mix-buffer offset length time)
  (declare (ignorable time)
           (optimize (speed 1) (debug 3))
           (type array-index offset length)
           (type sample-vector mix-buffer))  
  (update-for-seek streamer)
  (with-foreign-object (bitstream :int)
    (let* ((max-buffer-length 8192)
           (handle (vorbis-handle streamer))
           (channels (channels streamer))
           (frame-size (* 2 channels))
           (read-buffer (or (io-buffer streamer)
                            (setf (io-buffer streamer)
                                  (make-array max-buffer-length
                                              :element-type (ecase channels
                                                              (1 '(signed-byte 16))
                                                              (2 'stereo-sample))))))
           (convert-buffer (or (convert-buffer streamer)
                               (setf (convert-buffer streamer)
                                     (if (eql channels 2)
                                         read-buffer
                                         (make-array max-buffer-length 
                                                     :element-type 'stereo-sample))))))
      #+NIL
      (format *trace-output* "~&  streamer-mix-into vorbis-streamer offset=~A length=~A time=~A channels=~A~%"
              offset length time channels)
      ;(declare (type sample-vector read-buffer))
      (mixalot:with-array-pointer (bufptr read-buffer)
        (loop with end-output-index = (the array-index (+ offset length))
              with output-index = offset
              with nread = 0
              with samples-read = 0
              with chunk-size = 0
              while (< output-index end-output-index) do 
              (assert (not (zerop frame-size)))
              (setf chunk-size (min max-buffer-length (- end-output-index output-index))
                    nread (ov-read handle bufptr (* frame-size chunk-size) 0 2 1 bitstream)
                    samples-read (the array-index (/ nread frame-size)))

              (when (<= nread 0) (loop-finish))
              
              ;; Convert mono
              (when (eql 1 channels)
                (loop for index from 0 below samples-read
                   do (setf (aref convert-buffer index) 
                            (mixalot:mono->stereo (aref read-buffer index)))))
              ;; Mix into buffer 
              (loop for out-idx upfrom (the array-index output-index)
                    for in-idx upfrom 0
                    repeat samples-read
                    do (stereo-mixf (aref mix-buffer out-idx) 
                                    (aref convert-buffer in-idx)))
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
                         (vorbis-strerror nread ))
                 (mixer-remove-streamer mixer streamer))))))))

;;; Seek protocol

(defmethod streamer-seekable-p ((stream vorbis-streamer) mixer)
  (declare (ignore mixer))
  (ov-seekable (vorbis-handle stream)))

(defmethod streamer-length ((stream vorbis-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (length) stream 
    length))

(defmethod streamer-seek ((stream vorbis-streamer) mixer position 
                          &key &allow-other-keys)
  (declare (ignore mixer))
  (with-slots (seek-to sample-rate output-rate) stream
    (setf seek-to (floor (* sample-rate position) output-rate)))
  (values))

(defmethod streamer-position ((stream vorbis-streamer) mixer)
  (declare (ignore mixer))
  (with-slots (position) stream
    position))
