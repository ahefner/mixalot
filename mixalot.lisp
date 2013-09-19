;;;; Mixalot audio mixer for ALSA

;;;; Copyright (c) 2009,2010 Andy Hefner

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
  (:use :common-lisp :cffi :bordeaux-threads :mixalot-ffi-common)
  (:export #:alsa-error
           #:main-thread-init

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
           #:mixer-note-write
           #:create-mixer
           #:destroy-mixer
           #:array-index
           #:with-array-pointer
           #:clamp-sample #:clamp-sample+
           #:mono->stereo #:stereo-left #:stereo-right
           #:stereo->mono
           #:%stereo-left #:%stereo-right
           #:split-sample
           #:mix-stereo-samples #:add-stereo-samples
           #:stereo-incf #:stereo-mixf
           #:make-test-streamer
           #:dummy-mixer
           #:playback-finished

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
           #:make-fast-vector-streamer-joint-stereo

           #:vector-streamer-mono-single-float
           #:make-vector-streamer-mono-single-float
           #:vector-streamer-mono-double-float
           #:make-vector-streamer-mono-double-float))

(in-package :mixalot)

(eval-when (:compile-toplevel)
  #-linux (pushnew 'use-ao *features*)
  #+linux (pushnew 'use-alsa *features*))

(deftype array-index ()
  #-sbcl '(integer 0 #.array-dimension-limit)
  #+sbcl 'sb-int:index)

(deftype stereo-sample () '(unsigned-byte 32))

(deftype mono-sample ()
  '(or
    (signed-byte 16)
    (unsigned-byte 16)))

(deftype sample-vector () '(simple-array stereo-sample 1))

;;;; FFI to minimal subset of ALSA library

#+mixalot::use-alsa
(define-foreign-library libasound
    (t (:or "libasound.so.2" "libasound.so"
            "/usr/lib/libasound.so"
            "/usr/local/lib/libasound.so")))

#+mixalot::use-alsa
(use-foreign-library libasound)

#+mixalot::use-alsa
(define-condition alsa-error (error)
  ((text :initarg :text))
  (:documentation "An error from the ALSA library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

#+mixalot::use-alsa
(defcfun snd-strerror :string (errnum :int))

#+mixalot::use-alsa
(defun check-error (circumstance result)
  (unless (zerop result)
    (error 'alsa-error
           :text (format nil "~A: ~A" circumstance (snd-strerror result)))))

#+mixalot::use-alsa
(defctype snd-pcm :pointer)

#+mixalot::use-alsa
(defcenum snd-pcm-stream
    (:playback 0)
    (:capture 1))

#+mixalot::use-alsa
(defcenum snd-pcm-mode
    (:blocking 0)
    (:nonblocking 1)
    (:async 2))

#+mixalot::use-alsa
(defcfun (%snd-pcm-open "snd_pcm_open") :int
  (pcm (:pointer snd-pcm))
  (name :string)
  (type snd-pcm-stream)
  (mode snd-pcm-mode))

#+mixalot::use-alsa
(defun snd-pcm-open (name stream-type mode)
  (with-foreign-object (pcm 'snd-pcm)
    (check-error
     (format nil "PCM open of ~W (~A,~A)" name stream-type mode)
     (%snd-pcm-open pcm name stream-type mode))
    (validate-pointer (mem-ref pcm 'snd-pcm))))

#+mixalot::use-alsa
(defcfun snd-pcm-close :int
  (pcm snd-pcm))

#+mixalot::use-alsa
(defcenum snd-pcm-format
  (:snd-pcm-format-s16-le 2))

#+mixalot::use-alsa
(defcenum snd-pcm-access
    (:snd-pcm-access-rw-interleaved 3)
    (:snd-pcm-access-rw-noninterleaved 4))

#+mixalot::use-alsa
(defcfun snd-pcm-set-params :int
  (pcm           snd-pcm)
  (format        snd-pcm-format)
  (access        snd-pcm-access)
  (channels      :unsigned-int)
  (rate          :unsigned-int)
  (soft-resample :int)
  (latency       :unsigned-int))

#+mixalot::use-alsa
(defcfun snd-pcm-recover :int
  (pcm    snd-pcm)
  (err    :int)
  (silent :int))

#+mixalot::use-alsa (defctype snd-pcm-sframes :long)
#+mixalot::use-alsa (defctype snd-pcm-uframes :unsigned-long)

#+mixalot::use-alsa
(defcfun snd-pcm-writei snd-pcm-sframes
  (pcm    snd-pcm)
  (buffer :pointer)
  (size   snd-pcm-uframes))

#+mixalot::use-alsa (defctype snd-output :pointer)

#+mixalot::use-alsa
(defcfun snd-output-stdio-attach :int
  (outputp (:pointer snd-output))
  (file    :pointer)
  (close   :int))

#+mixalot::use-alsa
(defcfun snd-pcm-dump :int
  (pcm snd-pcm)
  (out snd-output))

#+mixalot::use-alsa
(defun main-thread-init ()
  ;; libao needs this. ALSA does not, which is the only good thing I can say about it.
  (values))

;;;; ALSA Utilities

#+mixalot::use-alsa (defcvar stdout :pointer)

#+mixalot::use-alsa
(defun dump-pcm-info (pcm)
  (with-foreign-object (output :pointer)
    (check-error
     "Attach output"
     (snd-output-stdio-attach output stdout 0))
    (check-error
     "PCM diagnostic state dump"
     (snd-pcm-dump pcm (mem-ref output :pointer)))))

#+mixalot::use-alsa
(defun call-with-pcm (rate continuation)
  (let ((pcm (snd-pcm-open "default" :playback :blocking)))
    (unwind-protect
         (progn
           (check-error
            "PCM set parameters"
            (snd-pcm-set-params
             pcm :snd-pcm-format-s16-le :snd-pcm-access-rw-interleaved
             2 rate 1 300000))
           (funcall continuation pcm))
      (snd-pcm-close pcm))))

;;;; Alternate interface using libao. Tested on OS X, FreeBSD.

;;; This isn't ideal. You're forced to initialize the audio system
;;; (and thus the mixer) from the "main thread", due to OS X being a
;;; half-assed joke.

;;; In theory this could work in Win32 as well. I haven't tried it.

#+mixalot::use-ao
(define-foreign-library libao
  (:darwin (:or "libao.4.dylib" "/opt/local/lib/libao.4.dylib"))
  (t (:or "libao.so")))

#+mixalot::use-ao (use-foreign-library libao)

;; Danger! This must be called from the main thread! Stupid OS X.
#+mixalot::use-ao
(defcfun ao-initialize :void)

#+mixalot::use-ao
(defcfun ao-default-driver-id :int)

#+mixalot::use-ao
(defcstruct (ao-sample-format :conc-name ao-fmt-)
  (bits :int)
  (rate :int)
  (channels :int)
  (byte-format :int)
  (matrix :string))

#+mixalot::use-ao (defconstant AO_FMT_LITTLE 1)
#+mixalot::use-ao (defconstant AO_FMT_BIG    2)
#+mixalot::use-ao (defconstant AO_FMT_NATIVE 4)

#+mixalot::use-ao (defctype ao-device* :pointer)

#+mixalot::use-ao
(defcfun ao-open-live ao-device*
  (driver-id :int)
  (format (:pointer ao-sample-format))
  (options :pointer))

#+mixalot::use-ao (defvar *ao-main-thread-init* nil)
#+mixalot::use-ao (defvar *aodev* nil)

#+mixalot::use-ao
(defun open-ao (&key (rate 44100))
  (unless *ao-main-thread-init*
    (error "libao not initialized. You must call MIXALOT:MAIN-THREAD-INIT from the main thread of your lisp. In SBCL, this will be the initial REPL (the *inferior-lisp* buffer in SLIME). If you call it from another thread, Lisp may crash."))
  (with-foreign-object (fmt 'ao-sample-format)
    (with-foreign-string (matrix "L,R")
     (setf (ao-fmt-bits fmt) 16
           (ao-fmt-channels fmt) 2
           (ao-fmt-rate fmt) rate
           (ao-fmt-byte-format fmt) AO_FMT_LITTLE
           (ao-fmt-matrix fmt) matrix)
    (ao-open-live (ao-default-driver-id)
                   fmt
                   (null-pointer)))))

#+mixalot::use-ao
(defun main-thread-init ()
  (unless *ao-main-thread-init*
    (setf *ao-main-thread-init* t)
    (ao-initialize)))

#+mixalot::use-ao
(defcfun ao-play :int
  (device ao-device*)
  (output-samples :pointer)
  (num-bytes :uint32))


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
  device)

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

(defgeneric mixer-remove-streamer (mixer streamer))

(defmethod mixer-remove-streamer ((mixer mixer) streamer)
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

(define-condition playback-finished ()
  ()
  (:documentation "Condition, which can be signalled by streamer to indicate, that it had finished its playback."))

(defgeneric mixer-note-write (mixer buffer offset size)
  (:method (mixer buffer offset size)
    (declare (ignore mixer buffer offset size))))

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
                  (handler-case
                      (funcall (if first
                                   #'streamer-write-into
                                   #'streamer-mix-into)
                               streamer
                               mixer
                               buffer
                               offset
                               (- buffer-samples offset)
                               (+ time offset))
                    (playback-finished () (mixer-remove-streamer mixer streamer)))
                (remove-streamer ()
                  :report "Delete this audio stream"
                  (mixer-remove-streamer mixer streamer))))
        ;; If there are no playable streams, we have to clear the buffer ourself.
        (when (and (zerop (length playable-streams))
                   (not buffer-clear))
          (fill buffer 0)
          (setf buffer-clear t))
        ;; Notification of data written.
        (mixer-note-write mixer buffer 0 buffer-samples)

        ;; Play the buffer.
        #+mixalot::use-ao
        (let ((ret
               (with-array-pointer (ptr buffer)
                 (ao-play (mixer-device mixer) ptr (* 4 buffer-samples)))))
          (when (zerop ret)
            (format *trace-output* "libao error.")))

        #+mixalot::use-alsa
        (loop with offset of-type mixer-buffer-index = 0
              as nwrite = (- buffer-samples offset)
              as nframes = (with-array-pointer (ptr buffer)
                             (incf-pointer ptr (* offset 4))
                             (snd-pcm-writei (mixer-device mixer) ptr nwrite))
              do
              (unless (zerop offset)
                (format t "~&mixer time ~A partial offset ~:D~%"
                        (mixer-current-time mixer)
                        offset))
              (assert (integerp nframes))
              (cond
                ((< nframes 0)
                 (format *trace-output* "~&nframes<0, snd-pcm-recover")
                 (snd-pcm-recover (mixer-device mixer) nframes 1))
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

(defun create-mixer (&key (rate 44100) (constructor 'make-mixer))
  "Create a new mixer at the specified sample rate, running in its own thread."
  #-(or mixalot::use-ao mixalot::use-alsa)
  (error "Neither mixalot::use-ao nor mixalot::use-alsa existed on *features* when this function was compiled. That is wrong.")
  (let ((mixer (funcall constructor :rate rate)))
    (bordeaux-threads:make-thread
     (lambda ()
       #+mixalot::use-ao
       (progn
         (setf (mixer-device mixer) (open-ao :rate rate))
         (run-mixer-process mixer))
       #+mixalot::use-alsa
       (call-with-pcm rate
        (lambda (pcm)
          (setf (mixer-device mixer) pcm)
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
                 stereo->mono %stereo-left %stereo-right
                 split-sample
                 mix-stereo-samples add-stereo-samples
                 scale-sample scale-stereo-sample))

(defun stereo-sample (left right)
  (declare (optimize (speed 3))
           (type mono-sample left right))
  (logior (ldb (byte 16 0) left)
          (ash (ldb (byte 16 0) right) 16)))

(defun mono->stereo (sample)
  (stereo-sample sample sample))

(defun sign-extend-16 (x)
  (declare (optimize (speed 3))
           (type (unsigned-byte 16) x))
  (let ((c (ash -1 15)))
    (logxor (+ x c) c)))

(defun %stereo-left (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (ldb (byte 16 0)  sample))

(defun %stereo-right (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (ldb (byte 16 16)  sample))

(defun stereo-left (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (sign-extend-16 (%stereo-left  sample)))

(defun stereo-right (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  (sign-extend-16 (%stereo-right sample)))

(defun stereo->mono (sample)
  (declare (optimize (speed 3))
           (type stereo-sample sample))
  ;; SBCL doesn't do the best job on this on.
  (ash (+ (stereo-left sample) (stereo-right sample)) -1))

(defun split-sample (sample)
  (values (stereo-left sample)
          (stereo-right sample)))

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

(defmacro define-vector-streamer (name &key type step sample-expr optimize)
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
                ,@(and optimize '((optimize (speed 3))))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream stereo-mixf ,type ,step ,sample-expr))
     (defmethod streamer-write-into ((stream ,name) mixer buffer offset length time)
       (declare (type array-index offset length)
                (type sample-vector buffer)
                ,@(and optimize '((optimize (speed 3))))
                (ignore time))
       (vector-stream-do-seek stream)
       (meta-vector-streamer stream setf ,type ,step ,sample-expr))))

(define-vector-streamer vector-streamer-mono
    :type vector
    :step 1
    :sample-expr (mono->stereo (aref vector vector-index)))

(define-vector-streamer vector-streamer-interleaved-stereo
    :type vector
    :step 2
    :sample-expr (stereo-sample (aref vector vector-index)
                                (aref vector (1+ vector-index))))

(define-vector-streamer vector-streamer-joint-stereo
    :type vector
    :step 1
    :sample-expr (aref vector vector-index))

(define-vector-streamer fast-vector-streamer-mono
    :type (simple-array (signed-byte 16) 1)
    :step 1
    :sample-expr (mono->stereo (aref vector vector-index)))

(define-vector-streamer fast-vector-streamer-interleaved-stereo
    :type (simple-array (signed-byte 16) 1)
    :optimize t
    :step 2
    :sample-expr (stereo-sample (aref vector vector-index)
                                (aref vector (1+ vector-index))))

(define-vector-streamer fast-vector-streamer-joint-stereo
    :type sample-vector
    :optimize t
    :step 1
    :sample-expr (aref vector vector-index))

(define-vector-streamer vector-streamer-mono-single-float
    :type (vector single-float)
    :step 1
    :sample-expr (mono->stereo (clamp-sample (round (* 32767.0f0 (aref vector vector-index))))))

(define-vector-streamer vector-streamer-mono-double-float
    :type (vector double-float)
    :step 1
    :sample-expr (mono->stereo (clamp-sample (round (* 32767.0d0 (aref vector vector-index))))))

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

(defclass dummy-mixer ()
  ((rate :initform 44100 :initarg :rate)
   (callback :initform nil :initarg :callback-on-streamer-remove :reader dummy-mixer-callback)))

(defmethod mixer-remove-streamer ((mixer dummy-mixer) streamer)
  (declare (ignore streamer))
  (funcall (dummy-mixer-callback mixer)))
