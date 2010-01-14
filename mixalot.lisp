;;;; Mixalot audio mixer for ALSA

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

(defpackage :mixalot
  (:use :common-lisp :cffi :bordeaux-threads)
  (:export #:alsa-error
           #:sample-vector
           #:stereo-sample
           #:mono-sample

           #:streamer-mix-into
           #:streamer-write-into
           #:streamer-cleanup
           #:streamer-pause
           #:streamer-unpause
           #:streamer-paused-p
           #:streamer-seekable-p
           #:streamer-length
           #:streamer-seek
           #:streamer-position
           #:streamer-note-completion

           #:mixer
           #:mixer-stream-lock
           #:mixer-stream-list
           #:mixer-current-time
           #:mixer-rate
           #:mixer-shutdown-flag
           #:mixer-add-streamer
           #:mixer-remove-streamer
           #:mixer-remove-all-streamers
           #:create-mixer
           #:destroy-mixer 
           #:array-index
           #:with-array-pointer
           #:clamp-sample #:clamp-sample+ 
           #:mono->stereo #:stereo-left #:stereo-right
           #:%stereo-left #:%stereo-right
           #:split-sample
           #:mix-stereo-samples #:add-stereo-samples
           #:stereo-incf #:stereo-mixf
           #:make-test-streamer

           #:vector-streamer
           #:vector-streamer-mono
           #:make-vector-streamer-mono
           #:vector-streamer-interleaved-stereo
           #:make-vector-streamer-interleaved-stereo
           #:vector-streamer-joint-stereo
           #:make-vector-streamer-joint-stereo
           #:fast-vector-streamer-mono
           #:make-fast-vector-streamer-mono
           #:fast-vector-streamer-interleaved-stereo
           #:make-fast-vector-streamer-interleaved-stereo
           #:fast-vector-streamer-joint-stereo
           #:make-fast-vector-streamer-joint-stereo))

(in-package :mixalot)

;;;; FFI to minimal subset of ALSA library

(define-foreign-library libasound
    (t (:or "libasound.so.2" "libasound.so"
            "/usr/lib/libasound.so"
            "/usr/local/lib/libasound.so")))

(use-foreign-library libasound)

(define-condition alsa-error (error)
  ((text :initarg :text))
  (:documentation "An error from the ALSA library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defcfun snd-strerror :string (errnum :int))

(defun check-error (circumstance result)
  (unless (zerop result)
    (error 'alsa-error 
           :text (format nil "~A: ~A" circumstance (snd-strerror result)))))

(defctype snd-pcm :pointer)

(defcenum snd-pcm-stream
    (:playback 0)
    (:capture 1))

(defcenum snd-pcm-mode
    (:blocking 0)
    (:nonblocking 1)
    (:async 2))

(defcfun (%snd-pcm-open "snd_pcm_open") :int
  (pcm (:pointer snd-pcm))
  (name :string)
  (type snd-pcm-stream)
  (mode snd-pcm-mode))

(defun valid-pointer (ptr) (unless (null-pointer-p ptr) ptr))

(defun validate-pointer (ptr)
  (or (valid-pointer ptr)
      (error "Unexpected NULL pointer")))

(defun snd-pcm-open (name stream-type mode)
  (with-foreign-object (pcm 'snd-pcm)
    (check-error
     (format nil "PCM open of ~W (~A,~A)" name stream-type mode)
     (%snd-pcm-open pcm name stream-type mode))
    (validate-pointer (mem-ref pcm 'snd-pcm))))

(defcfun snd-pcm-close :int 
  (pcm snd-pcm))

(defcenum snd-pcm-format 
  (:snd-pcm-format-s16-le 2))

(defcenum snd-pcm-access
    (:snd-pcm-access-rw-interleaved 3)
    (:snd-pcm-access-rw-noninterleaved 4))

(defcfun snd-pcm-set-params :int
  (pcm           snd-pcm)
  (format        snd-pcm-format)
  (access        snd-pcm-access)
  (channels      :unsigned-int)
  (rate          :unsigned-int)
  (soft-resample :int)
  (latency       :unsigned-int))

(defcfun snd-pcm-recover :int
  (pcm    snd-pcm)
  (err    :int)
  (silent :int))

(defctype snd-pcm-sframes :long)
(defctype snd-pcm-uframes :unsigned-long)

(defcfun snd-pcm-writei snd-pcm-sframes
  (pcm    snd-pcm)
  (buffer :pointer)
  (size   snd-pcm-uframes))

(defctype snd-output :pointer)

(defcfun snd-output-stdio-attach :int
  (outputp (:pointer snd-output))
  (file    :pointer)
  (close   :int))

(defcfun snd-pcm-dump :int 
  (pcm snd-pcm)
  (out snd-output))

(deftype array-index ()
  #-sbcl '(integer 0 #.array-dimension-limit)
  #+sbcl 'sb-int:index)

(deftype stereo-sample () '(unsigned-byte 32))

(deftype mono-sample () 
  '(or 
    (signed-byte 16)
    (unsigned-byte 16)))

(deftype sample-vector () '(simple-array stereo-sample 1))

;;;; ALSA Utilities

(defcvar stdout :pointer)

(defun dump-pcm-info (pcm)
  (with-foreign-object (output :pointer)
    (check-error
     "Attach output"
     (snd-output-stdio-attach output stdout 0))
    (check-error
     "PCM diagnostic state dump"
     (snd-pcm-dump pcm (mem-ref output :pointer)))))

(defun call-with-pcm (rate continuation)
  (let ((pcm (snd-pcm-open "default" :playback :blocking)))
    (unwind-protect 
         (progn
           (check-error
            "PCM set parameters"
            (snd-pcm-set-params
             pcm :snd-pcm-format-s16-le :snd-pcm-access-rw-interleaved
             2 rate 1 100000))
           (funcall continuation pcm))
      (snd-pcm-close pcm))))


;;;; Basic stream protocol

(defgeneric streamer-mix-into (stream mixer buffer offset length time)
  (:documentation
   "Mix 'length' samples of stream output into buffer starting at 'offset'
 measured in samples, at 'time' (measured in samples since the mixer was
 created. The time measurement includes the offset, and is intended for
 synchronizing streams. Called from outside the mixer lock.")
  (:method ((stream function) mixer buffer offset length time)
    (funcall stream stream mixer buffer offset length time)))

(defgeneric streamer-write-into (stream mixer buffer offset length time)
  (:documentation
   "Write 'length' samples of stream output into buffer starting at
   'offset' (measured in samples), at 'time' (measured in samples
   since the mixer was created. The time measurement includes the
   offset, and is intended for synchronizing streams. The differs from
   stream-write-info in that you don't have to mix the data, the
   current contents are expected to be garbage and can be
   overwritten. Implementing this is optional. Called from outside the
   mixer lock.")
  (:method (stream mixer buffer offset length time)
    (declare (type sample-vector buffer)
             (type array-index offset length)
             (optimize (speed 3)))
    (fill buffer 0 :start offset :end (+ offset length))
    (streamer-mix-into stream mixer buffer offset length time)))

(defgeneric streamer-cleanup (stream mixer)
  (:documentation 
  "Release resources and perform any other cleanups needed when a
  streamer is destroyed as a result of a call to mixer-remove-streamer. 
  Called outside the mixer lock, so it's okay to manipulate the mixer.")
  (:method (stream mixer)
    (declare (ignore stream mixer))))

;;;; Pausing streams: The mixer handles pausing automatically.
;;;; Streamers need not define methods on these functions, unless they
;;;; have a reason to take action on pause/unpause.

(defgeneric streamer-pause (stream mixer)
  (:documentation "Pause playback of the streamer. A method on
  streamer-pause is optional and serves as a notification to the
  streamer that it has been paused; the default method is specialized
  on the mixer and can suspend playback without any special support
  from the streamer."))

(defgeneric streamer-unpause (stream mixer)
  (:documentation "Unpause playback of the streamer. A method on
  streamer-unpause is optional and serves as a notification to the
  streamer that it has been unpaused; the default method is
  specialized on the mixer and can resume playback without any special
  support from the streamer."))

(defgeneric streamer-paused-p (stream mixer)
  (:documentation "Query whether a stream is paused or not."))

;;;; Optional: Seekable stream protocol

(defgeneric streamer-seekable-p (stream mixer)
  (:documentation "Returns non-NIL if the streamer supports seeking.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))

(defgeneric streamer-length (stream mixer)
  (:documentation "Returns length, in samples, of the audio stream, or
  NIL if it cannot be determined.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))

(defgeneric streamer-seek (stream mixer position &key &allow-other-keys)
  (:documentation "Seek to position (measured in samples) from the start of stream."))

(defgeneric streamer-position (stream mixer)
  (:documentation "Returns current position within a seekable stream.")
  (:method (stream mixer)
    (declare (ignore stream mixer))
    nil))


;;;; Mixer process

;;; The mixer contains zero or more streamers and pumps samples from
;;; them to mix and send to the audio card. Streamers can be added and
;;; removed at (almost) any time.

;;; Another reasonable design would be that you connect a single
;;; stream to the audio device, and a mixer is just another type of
;;; stream. This would solve some problems for a certain kind of app,
;;; but I haven't pursued simply because I didn't think of it soon
;;; enough, and changing to that approach if/when I have use for it
;;; shouldn't break the API incompatibly.

(defstruct mixer 
  (stream-lock (bordeaux-threads:make-lock "Mixer lock"))
  (stream-list  nil)
  (current-time 0)
  (rate         44100)
  (shutdown-flag nil)
  (stream-state (make-hash-table))
  pcm-instance)

(defmacro with-mixer-lock ((mixer) &body body)
  `(with-lock-held ((mixer-stream-lock ,mixer))
    ,@body))

(defun mixer-add-streamer (mixer streamer)
  (with-mixer-lock (mixer)
    (cond
      ((mixer-shutdown-flag mixer)
       (error "You can't add a stream to a shutdown mixer!"))
      (t (push streamer (mixer-stream-list mixer))
         (values streamer (mixer-current-time mixer))))))

(defun %req-remove-streamer (mixer streamer)
  (setf (gethash streamer (mixer-stream-state mixer)) :remove))

(defun mixer-remove-streamer (mixer streamer)
  (with-mixer-lock (mixer)
    (%req-remove-streamer mixer streamer))
  (values))

(defun mixer-remove-all-streamers (mixer)
  (with-mixer-lock (mixer)
    (dolist (streamer (mixer-stream-list mixer))
      (%req-remove-streamer mixer streamer))))      

;;; Obtaining a pointer to an array of unboxed data. I used to do this
;;; myself, but recentish CFFI can do it for me.
(defmacro with-array-pointer ((name array) &body body)
  `(cffi-sys:with-pointer-to-vector-data (,name ,array) ,@body))

#+NIL
(defmacro with-array-pointer ((name array) &body body)
  ;; Perhaps does the wrong thing for displaced arrays.
  ;; This will never affect me.
  ;; Also, SBCL gives a very bizarre code deletion warning here
  ;; when compiling the file in SLIME which goes away when I 
  ;; compile just the definition.
  `((lambda (arrayoid body)
      (unless (typep arrayoid 'vector)
        (setf arrayoid (sb-kernel:%array-data-vector arrayoid)))
      (sb-sys:with-pinned-objects (arrayoid)
        (funcall body (sb-sys:vector-sap arrayoid))))
    ,array
    (lambda (,name) ,@body)))

(defmethod streamer-pause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (find stream (mixer-stream-list mixer))
      (setf (gethash stream (mixer-stream-state mixer)) :paused))))

(defmethod streamer-unpause (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (when (eql (gethash stream (mixer-stream-state mixer)) :paused)
      (remhash stream (mixer-stream-state mixer)))))

(defmethod streamer-paused-p (stream (mixer mixer))
  (with-mixer-lock (mixer)
    (eql (gethash stream (mixer-stream-state mixer)) :paused)))

(defun update-playable (mixer playable-streams)
  (with-mixer-lock (mixer)
    (setf (fill-pointer playable-streams) 0)
    (dolist (stream (mixer-stream-list mixer))
      (let ((state (gethash stream (mixer-stream-state mixer))))
        (unless (eql :paused state)
          (vector-push-extend stream playable-streams))))))

(defun remove-removable (mixer temp-vector)  
  (with-mixer-lock (mixer)
    (let ((state-table (mixer-stream-state mixer)))
      (setf (fill-pointer temp-vector) 0
            (mixer-stream-list mixer) 
            (delete-if
             (lambda (streamer)
               (when (eql :remove (gethash streamer state-table))
                 (vector-push-extend streamer temp-vector)
                 (remhash streamer state-table)
                 t))
             (mixer-stream-list mixer)))))
  ;; Run the cleanups outside the lock:
  (loop for removed across temp-vector
        do (streamer-cleanup removed mixer)))

(defconstant +mixer-buffer-size+ 4096)
(deftype mixer-buffer-index () `(integer 0 ,+mixer-buffer-size+))

(defun run-mixer-process (mixer)
 (declare (optimize (speed 3)))
 (unwind-protect
  ;; Body
  (loop with time = 0
        with buffer-samples = +mixer-buffer-size+
        with buffer = (make-array buffer-samples :element-type '(unsigned-byte 32))
        with playable-streams = (make-array 0 :adjustable t :fill-pointer 0)
        with buffer-clear = nil
        until (mixer-shutdown-flag mixer)
        do
        ;; So that we don't have to hold the lock during the stream
        ;; callbacks, use this temporary vector:
        (remove-removable mixer playable-streams)
        (update-playable mixer playable-streams)
        ;; Loop through playable streams and generate audio
        (loop for streamer across playable-streams
              for first = t then nil
              as offset = 0             ; ...
              do
              (setf buffer-clear nil)
              (restart-case
                  (funcall (if first
                               #'streamer-write-into
                               #'streamer-mix-into)
                           streamer
                           mixer
                           buffer
                           offset
                           (- buffer-samples offset)
                           (+ time offset))
                (remove-streamer ()
                  :report "Delete this audio stream"
                  (mixer-remove-streamer mixer streamer))))
        ;; If there are no playable streams, we have to clear the buffer ourself.
        (when (and (zerop (length playable-streams))
                   (not buffer-clear))
          (fill buffer 0)
          (setf buffer-clear t))
        ;; Play the buffer.
        (loop with offset of-type mixer-buffer-index = 0
              as nwrite = (- buffer-samples offset)
              as nframes = (with-array-pointer (ptr buffer)
                             (incf-pointer ptr (* offset 4))
                             (snd-pcm-writei (mixer-pcm-instance mixer) ptr nwrite))
              do
              (unless (zerop offset)
                (format t "~&mixer time ~A partial offset ~:D~%"
                        (mixer-current-time mixer)
                        offset))
              (cond
                ((not (integerp nframes)) (error "wtf"))
                ((< nframes 0)
                 (format *trace-output* "~&nframes<0, snd-pcm-recover")
                 (snd-pcm-recover (mixer-pcm-instance mixer) nframes 1))
                ((< nframes nwrite)
                 (format *trace-output* "~&short write ~D vs ~D (offset ~D)~%"
                         nframes (- buffer-samples offset) offset)
                 (incf offset nframes))
                (t (loop-finish))))

        (incf time buffer-samples)
        (setf (mixer-current-time mixer) time))
   ;; Cleanup. After setting the shutdown flag, it is impossible to
   ;; add additional streamers, so there's no race during the shutdown.
   (with-mixer-lock (mixer) (setf (mixer-shutdown-flag mixer) t))
   (dolist (streamer (mixer-stream-list mixer))
     (streamer-cleanup streamer mixer))
   (clrhash (mixer-stream-state mixer))
   (setf (mixer-stream-list mixer) nil)))


(defun create-mixer (&key (rate 44100))
  "Create a new mixer at the specified sample rate, running in its own thread."
  (let ((mixer (make-mixer :rate rate)))
    (bordeaux-threads:make-thread
     (lambda ()
       (call-with-pcm rate
        (lambda (pcm)
          (setf (mixer-pcm-instance mixer) pcm)
          (run-mixer-process mixer))))
     :name (format nil "Mixer thread ~:D Hz" rate))
    mixer))

(defun destroy-mixer (mixer)
  (with-mixer-lock (mixer)
    (setf (mixer-shutdown-flag mixer) t))
  (values))

;;;; Fastish sample manipulation

(declaim (inline stereo-sample sign-extend-16
                 clamp-sample clamp-sample+ 
                 mono->stereo stereo-left stereo-right
                 %stereo-left %stereo-right
                 split-sample
                 mix-stereo-samples add-stereo-samples
                 scale-sample scale-stereo-sample))

(defun stereo-sample (left right)
  (declare (optimize (speed 3))
           (type mono-sample left right))
  (logior (ldb (byte 16 0) left)
          (ash (ldb (byte 16 0) right) 16)))

(defun mono->stereo (sample) (stereo-sample sample sample))

(defun sign-extend-16 (x)
  (declare (optimize (speed 3))
           (type (unsigned-byte 16) x))
  (let ((c (ash -1 15)))
    (logxor (+ x c) c)))  

(defun %stereo-left (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (ldb (byte 16 0)  sample))

(defun %stereo-right (sample) 
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (ldb (byte 16 16)  sample))

(defun stereo-left (sample)
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (sign-extend-16 (%stereo-left  sample)))

(defun stereo-right (sample) 
  (declare (optimize (speed 3)) (type stereo-sample sample))
  (sign-extend-16 (%stereo-right sample)))

(defun split-sample (sample)
  (values (stereo-left sample) (stereo-right sample)))

(defun clamp-sample (x) (min 32767 (max -32768 x)))
(defun clamp-sample+ (x y) (clamp-sample (+ x y)))

(defun mix-stereo-samples (x y)
  "Mix two stereo samples by clamped addition"
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (stereo-sample (clamp-sample+ (stereo-left  x) (stereo-left  y))
                 (clamp-sample+ (stereo-right x) (stereo-right y))))

(defun add-stereo-samples (x y) 
  "Add two stereo samples, without clipping."
  (declare (optimize (speed 3)) (type stereo-sample x y))
  (logior (logand #xFFFF (+ x y))
          (logand #xFFFF0000 (+ x (logand #xFFFF0000 y))))
  #+NIL ;; Equivalent, slower version:
  (stereo-sample (logand #xFFFF (+ (%stereo-left x) (%stereo-left y)))
                 (logand #xFFFF (+ (%stereo-right x) (%stereo-right y)))))

(defun scale-sample (x y)
  (declare (optimize (speed 3))
           (type mono-sample x y))
  (ash (* x y) -16))

(defun scale-stereo-sample (stereo scale)
  (declare (optimize (speed 3))
           (type stereo-sample stereo)
           (type (signed-byte 16) scale))
  #+NIL
  (logior (logand #xFFFF0000 (* scale (ash stereo -16)))
          (logand #x0000FFFF (ash (* (logand stereo #xFFFF) scale) -16)))

  (stereo-sample (scale-sample (stereo-left  stereo) scale)
                 (scale-sample (stereo-right stereo) scale)))

(define-modify-macro stereo-incf (sample) add-stereo-samples)
(define-modify-macro stereo-mixf (sample) mix-stereo-samples)

;;;; Testing streamer

(defun make-test-streamer ()
  (let ((n 0)
        (phase 0.0))
    (lambda (streamer mixer buffer offset length time)
      (declare (ignore time))
      (loop for index upfrom offset
            repeat length
            with freq = (+ 200 (* n 200))
            with dp = (* 2.0 pi freq 1/44100)
            as sample = (round (* 5000 (sin phase)))
            do            
            (stereo-incf (aref buffer index) (mono->stereo sample))
            (incf phase dp))
      (incf n)
      (when (= n 6)
        (mixer-remove-streamer mixer streamer)))))

;;;; Streamers for pre-filled vectors of various formats.

(defmacro meta-vector-streamer (vstreamer modifier type step sample-expr)
  `(with-slots (vector position end) ,vstreamer
     (declare (type array-index end)
              (type ,type vector))
       (loop for vector-index from (the array-index position) by ,step below end
             for output-index from offset below (+ offset length)
             do (,modifier (aref buffer output-index) ,sample-expr)
             finally (setf position vector-index))
       (when (= (the array-index position) end)
         (mixer-remove-streamer mixer ,vstreamer))))
  
(defclass vector-streamer ()
  ((vector :reader vector-of :initarg :vector)
   (start  :reader start     :initarg :start)
   (end    :reader end       :initarg :end)
   (position :reader position-of :initarg :position)
   (elts-per-sample :reader elts-per-sample :initarg :elts-per-sample)
   (seek-to :accessor seek-to :initform nil)))

(defun vector-stream-do-seek (stream)
  (when (seek-to stream)
    (setf (slot-value stream 'position) (seek-to stream)
          (seek-to stream) nil)))

(defmacro define-vector-streamer (name type step sample-expr)
  `(progn
     (defclass ,name (vector-streamer) ())
     (defun ,(intern (format nil "MAKE-~A" (symbol-name name)))
         (vector &optional (start 0) (end (length vector)))
       (make-instance ',name :vector vector 
                      :start start :end end 
                      :position start
                      :elts-per-sample ,step))
     (defmethod streamer-mix-into ((stream ,name) mixer buffer offset length time)
       (declare (type array-index offset length)
                (type sample-vector buffer)
                (optimize (speed 3))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream stereo-mixf ,type ,step ,sample-expr))
     (defmethod streamer-write-into ((stream ,name) mixer buffer offset length time)
       (declare (type array-index offset length)
                (type sample-vector buffer)
                (optimize (speed 3))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream setf ,type ,step ,sample-expr))))

(define-vector-streamer  vector-streamer-mono
    vector 1 (mono->stereo (aref vector vector-index)))

(define-vector-streamer  vector-streamer-interleaved-stereo
    vector 2 (stereo-sample (aref vector vector-index)
                            (aref vector (1+ vector-index))))

(define-vector-streamer  vector-streamer-joint-stereo
    vector 1 (aref vector vector-index))

(define-vector-streamer  fast-vector-streamer-mono
    (simple-array (signed-byte 16) 1) 1 
    (mono->stereo (aref vector vector-index)))

(define-vector-streamer  fast-vector-streamer-interleaved-stereo 
    (simple-array (signed-byte 16) 1) 2 
    (stereo-sample (aref vector vector-index)
                   (aref vector (1+ vector-index))))

(define-vector-streamer  fast-vector-streamer-joint-stereo
    sample-vector 1 (aref vector vector-index))

(defmethod streamer-seekable-p ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  t)

(defmethod streamer-length ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  (/ (- (end stream) (start stream))
     (elts-per-sample stream)))

;;; There's a race condition here where a seek on a vector stream can,
;;; with very small probability, be ignored if there's already a
;;; previous seek pending, but I don't think it's worth coding around.
(defmethod streamer-seek
    ((stream vector-streamer) mixer position &key &allow-other-keys)
  (declare (ignore mixer))
  (setf (seek-to stream) (min (- (end stream) (elts-per-sample stream))
                              (max (start stream)
                                   (+ (* (elts-per-sample stream)
                                         position)
                                      (start stream))))))

(defmethod streamer-position ((stream vector-streamer) mixer)
  (declare (ignore mixer))
  (floor (- (position-of stream)
            (start stream))
         (elts-per-sample stream)))
