#!/usr/local/bin/sbcl --script

;;;; Shuffletron, the Mixalot music player example.

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

;;;; I strongly recommend running this inside 'rlwrap' (see shuffletron.sh)

;;;; Why did I write this as a shebang script, you ask?  I'm not
;;;; really sure myself. Because I could? In practice I still develop
;;;; it from within SLIME by first running the 'swankme' command to
;;;; start a swank server.

(in-package :cl-user)

;;; Silence compiler output:
(defvar *real-output*   *standard-output*)
(setf *standard-output* (make-broadcast-stream))
;(setf *error-output*    (make-broadcast-stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)
  (require :mixalot-mp3))

(defpackage :shuffletron
  (:use :common-lisp :mixalot :mixalot-mp3))

(in-package :shuffletron)

;;;; POSIX directory walker

(defmacro with-posix-interface (() &body body)
  `(let ((sb-alien::*default-c-string-external-format* :latin1))
    ,@body))

(defun %list-directory (path)
  (with-posix-interface ()
    (let ((dir (sb-posix:opendir path)))
      (unwind-protect 
	   (loop as dirent = (sb-posix:readdir dir)
		 until (sb-alien:null-alien dirent)
		 collect (sb-posix:dirent-name dirent))
	(sb-posix:closedir dir)))))

(defun list-directory (path)
  (delete "." (delete ".." (%list-directory path) :test #'string=)
	  :test #'string=))

(defun dfn (a b)
  (declare (type string a b))
  (if (and (char= #\/ (elt a (1- (length a))))
	   (zerop (length b)))
      a
      (concatenate 'string a (if (char= #\/ (elt a (1- (length a)))) "" "/") b)))

(defun abs-sorted-list-directory (path)
  (mapcar (lambda (filename) (dfn path filename))
          (sort (loop for filename in (list-directory path) collect filename) #'string<=)))

(defun walk (filename fn)
  "Walk directory tree, ignoring symlinks."
  (with-posix-interface ()
    (let* ((stat (sb-posix:lstat filename))
	   (mode (sb-posix:stat-mode stat)))
      (cond ((sb-posix:s-isdir mode) (dolist (f (abs-sorted-list-directory filename)) (walk f fn)))
	    ((sb-posix:s-isreg mode) (funcall fn filename)))))
  (values))

(defun rel (path filename)
  (let ((index (mismatch (dfn path "") filename)))
    (if (zerop index)
	(error "File ~A is not in path ~A" filename path)
        (subseq filename index))))

;;;; Preferences

(defun file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil))
        (read in)))))

(defsetf file (filename) (object)
 `(with-open-file (out ,filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-standard-io-syntax ()
     (pprint ,object out))))

(defun prefpath (name)
  (merge-pathnames
   (make-pathname :directory '(:relative ".shuffletron") 
                  :name (and name (string-downcase (string name))))
   (user-homedir-pathname)))

(defun pref (name &optional default)
  (handler-case (values (file (prefpath name)) t)
    (file-error (c)
      (when (probe-file (prefpath name))
        (format t "Problem reading ~A:~%~A~%" (prefpath name) c))
      (values default nil))
    (reader-error (c)
      (format t "Error parsing contents of ~A:~%~A~%" (prefpath name) c)
      (values default nil))))

(defun (setf pref) (value name)
  (ensure-directories-exist (prefpath nil))
  (setf (file (prefpath name)) value))


;;;; Library

(defvar *library* nil)
;;(defvar *library-base* nil)
(define-symbol-macro *library-base* (pref :library-base))

(defstruct song full-path local-path tags smashed highlight)

(defun init-library ()
  (setf *library* (make-array 0 :fill-pointer 0 :adjustable t)))

(defun mp3-p (filename)
  (not (mismatch filename "mp3" :test #'char-equal :start1 (- (length filename) 3))))

(defvar *library-progress*)

(defun smash-string (string)
  (substitute #\Space #\_ (string-downcase string)))

(defun add-mp3-file (full-filename relative-filename)
  (incf *library-progress*)
  (when (zerop (mod *library-progress* 10))
    (format t "~C" (code-char 13))
    (format t "Scanning. ~:D files.." *library-progress*))
    (finish-output)
  (vector-push-extend (make-song :full-path full-filename
                                 :local-path relative-filename
                                 :smashed (smash-string relative-filename)
                                 :highlight (make-array (length relative-filename)
                                                        :element-type 'bit)
                                 :tags nil
                                 #+NIL
                                 (handler-case (mpg123:get-tags-from-file full-filename)
                                   (error (c)
                                     (format t "~&Error scanning ~A:~%  ~A~%" full-filename c))))
                      *library*))

(defun library-scan (path)
  (let ((*library-progress* 0))
    (when (probe-file path)
      (walk path
            (lambda (filename)
              (when (mp3-p filename)
                (add-mp3-file filename (rel path filename))))))
    t))

;;;; Audio

(defvar *mixer* nil)

(defclass mp3-jukebox-streamer (mp3-streamer)
  ((song :accessor song-of :initarg :song)
   (stopped :accessor stopped :initform nil)
   (enqueue-on-completion :accessor enqueue-on-completion 
                          :initform nil
                          :initarg :enqueue-on-completion)))

(defun audio-init ()
  (setf *mixer* (create-mixer :rate 44100)))

;;;; State

(defvar *debug-mode* nil)

(defvar *selection* nil)
(defvar *selection-changed* nil)
(defvar *selection-history* nil)
(defvar *history-depth* 16)

(defun querying-library-p () (= (length *selection*) (length *library*)))

(defun set-selection (new-selection)
  (push *selection* *selection-history*)
  (when (> (length *selection-history*) *history-depth*)
    (setf *selection-history* (subseq *selection-history* 0 *history-depth*)))
  (setf *selection* new-selection
        *selection-changed* t)
  (values))

(defun reset-query ()
  (set-selection (copy-seq *library*))
  (loop for x across *library* do (fill (song-highlight x) 0)))

(defun refine-query (substring)
  ;; Query and update highlighting:
  (loop for song across *selection*
        with query-string = (smash-string substring)
        with new-selection = (make-array 0 :adjustable t :fill-pointer 0)
        as found = (search query-string (song-smashed song))

        when found do
        (fill (song-highlight song) 0)
        (fill (song-highlight song) 1 
              :start found 
              :end (+ found (length query-string)))
        (vector-push-extend song new-selection)

        finally (set-selection new-selection)))

;;; Lock discipline: If you need both locks, take the playqueue lock first.

(defvar *cslock* (bordeaux-threads:make-lock "current stream lock"))
(defvar *i-took-the-cs-look* nil)
(defvar *current-stream* nil)

(defmacro with-stream-control (() &body body)
  `(let ((*i-took-the-cs-look* t))
     (bordeaux-threads:with-lock-held (*cslock*) ,@body)))

(defvar *pqlock* (bordeaux-threads:make-lock "play queue lock"))
(defvar *playqueue* nil)

(defvar *loop-mode* nil)

(defvar *wakeup-time* nil
  "Time to wake up if alarm clock is enabled.")

(defmacro with-playqueue (() &body body)
  `(bordeaux-threads:with-lock-held (*pqlock*)
     (when *i-took-the-cs-look* 
       (format t "~&You took the PQ lock inside the CS lock. Don't do that.~%")
       (sb-debug:backtrace))
     ,@body))

(defun end-stream (stream)
  ;; TODO: Fade out nicely.
  (setf (stopped stream) t)
  (mixer-remove-streamer *mixer* stream)
  (setf *current-stream* nil))

(defun play-song (song &key enqueue-on-completion)
  (with-stream-control ()
    (when *current-stream* (end-stream *current-stream*))
    (let ((new (make-mp3-streamer (song-full-path song)                                  
                                  :class 'mp3-jukebox-streamer
                                  :song song
                                  :enqueue-on-completion enqueue-on-completion)))
      (setf *current-stream* new)
      (mixer-add-streamer *mixer* *current-stream*))))

(defun play-next-song ()
  (with-playqueue ()
    (cond
      (*playqueue* 
       (let ((next (pop *playqueue*)))
         (play-song next)
         (when *loop-mode*
           (setf *playqueue* (append *playqueue* (list next))))))
      (t (with-stream-control ()
           (when *current-stream* (end-stream *current-stream*)))))))

(defmethod streamer-cleanup ((stream mp3-jukebox-streamer) mixer)
  (call-next-method)
  ;; If stopped is set, someone else can be expected to be starting up
  ;; the next song. Otherwise, we have to do it ourselves.
  (unless (stopped stream)
    ;; If the song completed 
    (when (enqueue-on-completion stream)
      (with-playqueue ()
        (setf *playqueue* (append *playqueue* (list (song-of stream))))))
    (with-stream-control ()
      (when (eq stream *current-stream*)
        (setf *current-stream* nil)))
    ;; We do this call in new thread, because we are in the mixer
    ;; thread here, and scanning the next file could take long enough
    ;; to stall it.
    (bordeaux-threads:make-thread (lambda () (play-next-song)))))

(defun toggle-pause ()
  (with-stream-control ()
    (when *current-stream*
      (if (streamer-paused-p *current-stream* *mixer*)
          (streamer-unpause *current-stream* *mixer*)
          (streamer-pause *current-stream* *mixer*)))))

(defun unpause ()
  (with-stream-control ()
    (when *current-stream*
      (when (streamer-paused-p *current-stream* *mixer*)
        (streamer-unpause *current-stream* *mixer*)
        t))))

(defun current-song-playing ()
  (let ((stream *current-stream*))
    (and stream (song-of stream))))

(defun playqueue-and-current ()
  (let ((current (current-song-playing)))
    (if current
        (cons current *playqueue*)
        *playqueue*)))

(defun rescan-library ()
  (init-library)
  (library-scan *library-base*)
  (format t "~CLibrary contains ~:D files.        ~%" 
          (code-char 13) (length *library*)))

;;;; UI

(defparameter *max-query-results* 50
  "Maximum number of resuls to print without an explicit 'show' command.")

(defun parse-ranges (string start max)
  "Parse comma delimited numeric ranges, returning a list of min/max
pairs as cons cells"
  (when (or (>= start (length string))
            (char= #\- (aref string start)))
    (return-from parse-ranges nil))  
  (labels ((clamp (x) (max 0 (min x max)))
           (range (x y) (cons (clamp (min x y)) (clamp (max x y)))))
    (multiple-value-bind (min idx)
        (parse-integer string :junk-allowed t :start start)      
      (cond
        ((null min) nil)
        ((= idx (length string)) (list (range min min)))
        ((or (char= #\, (aref string idx))
             (char= #\  (aref string idx)))
         (list* (range min min)
                (parse-ranges string (1+ idx) max)))
      ((char= #\- (aref string idx))
       (multiple-value-bind (parsed-max idx)
           (parse-integer string :junk-allowed t :start (1+ idx))
         (list* (range min (or parsed-max max))
                (parse-ranges string (1+ idx) max))))
      (t (print '???) nil)))))

(defun expand-ranges (ranges)
  (loop for (min . max) in ranges
        nconcing (loop for i from min upto max collect i)))

(defun extract-ranges (vector rangespec-string)
  (map 'vector (lambda (i) (aref vector i))
       (expand-ranges (parse-ranges rangespec-string 0 (1- (length vector))))))

(defun spooky-init ()
  (setf cl-user::*real-output* 
        (sb-sys:make-fd-stream 1 :external-format :utf8 :output t :input nil))  
  (setf *standard-output* cl-user::*real-output*)
  (setf *error-output* cl-user::*real-output*)
  (setf *terminal-io* cl-user::*real-output*)
  (setf *query-io* cl-user::*real-output*)
  (sb-sys:enable-interrupt sb-unix:sigint 
    (lambda (&rest args) (declare (ignore args)) (sb-ext:quit))))

(defun getline ()
  (or (read-line *standard-input* nil) (sb-ext:quit)))

(defun init ()
  (format t "~&This is Shuffletron.~%")
  (setf *random-state* (make-random-state t))
  (loop do
        (init-library)
        (unless *library-base* 
          (format t "~&Enter library path: ")
          (finish-output)
          (setf *library-base* (dfn (getline) "")))
        (when (not (library-scan *library-base*))
          (format t "Unable to scan \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        (when (zerop (length *library*))
          (format t "No playable files found in \"~A\"~%" *library-base*)
          (setf *library-base* nil))
        until *library-base*)
  (format t "~CLibrary contains ~:D files.        ~%"
          (code-char 13) (length *library*))
  (reset-query))

(defun underline () (format t "~C[4m" #\Esc))
(defun bold      () (format t "~C[1m" #\Esc))
(defun roman     () (format t "~C[0m" #\Esc))

(defun build-sequence-table (seq)
  (let ((table (make-hash-table)))
    (map nil (lambda (elt) (setf (gethash elt table) elt)) seq)
    table))

(defun show-song-matches (items)
  (loop with hash = (build-sequence-table (playqueue-and-current))
        for item across items
        for n upfrom 0 do 
        (format t " ~7D~C " n (if (gethash item hash) #\* #\Space))
        (loop for char across (song-local-path item)
              for new-style across (song-highlight item)
              with current-style = 0
              do
              (unless (= new-style current-style)
                (if (zerop new-style)
                    (roman)
                    (underline))
                (setf current-style new-style))
              (write-char char)
              finally (roman))
        (terpri)))

(defun show-current-query ()
  (if (zerop (length *selection*))
      (format t "  Nothing matches the current query.~%")
      (show-song-matches *selection*)))

(defun selection-songs (rangespec)
  (if (zerop (length *selection*))
      (vector)
      (extract-ranges *selection* rangespec)))

(defun time->string (seconds)
  (setf seconds (round seconds))
  (if (>= seconds 3600)      
      (format nil "~D:~2,'0D:~2,'0D" (truncate seconds 3600) (mod (truncate seconds 60) 60) (mod seconds 60))
      (format nil "~D:~2,'0D" (truncate seconds 60) (mod seconds 60))))

(defun show-current-song (&optional delimit)
  (let ((current *current-stream*))
      (when current
        (when delimit (terpri))
        (let ((pos (streamer-position current *mixer*))
              (len (streamer-length   current *mixer*)))
          (format t "[~A/~A] ~A ~A~%"
                  (time->string (round pos (mixer-rate *mixer*)))
                  (time->string (round len (mixer-rate *mixer*)))
                  (if (streamer-paused-p current *mixer*)
                      "Paused:"
                      "Playing:")
                  (song-local-path (song-of current))))
        (when delimit (terpri)))))


(defun show-playqueue ()
  (with-playqueue ()
    (cond
      ((zerop (length *playqueue*))
       (format t "  The queue is empty.~%"))
      (t (loop for song in *playqueue*
               for i upfrom 0
               do (format t " ~7<(~D)~>  ~A~%" i (song-local-path song)))))
    (when *loop-mode* (format t "Loop mode enabled.~%"))
    (show-current-song t)))

;;; Awful anaphora in these parsing macros, they often assume IN
;;; is the name of the stream variable.

(defmacro parsing ((&optional string) &body body)
  (if string
      `(with-input-from-string (in ,string) (catch 'fail ,@body))
      `(catch 'fail ,@body)))


;;; Beware disjunctive definitions where branches are prefixes of
;;; other branches.  The first match will be accepted, and there's no
;;; backtracking if that was the wrong one.
(defmacro disjunction ((&optional string) &body branches)
  (if string
      `(or ,@(loop for branch in branches collect `(parsing (,string) ,branch)))
      (let ((start (gensym)))
        `(let ((,start (file-position in)))
           (or ,@(loop for branch in branches 
                       collect `(progn 
                                  (assert (file-position in ,start))
                                  (parsing (,string) ,branch))))))))

(defun val (x) (or x (throw 'fail nil)))

(defun num (in)
  (loop with accum = nil
        as next = (peek-char nil in nil)
        as digit = (and next (digit-char-p next 10))
        while digit do
        (read-char in)
        (setf accum (+ digit (* (or accum 0) 10)))
        finally (return (val accum))))

(defun colon (in) (val (eql #\: (read-char in nil))))
(defun mod60 (in) (let ((n (num in))) (val (and (< n 60) n))))
(defun eof (in) (val (not (peek-char nil in nil))))
(defun whitespace (in) (val (peek-char t in nil)))
(defun match (in match)
  (every (lambda (x) (val (char-equal x (val (read-char in nil))))) match))

(defun parse-timespec (string)
  "Parse a time/duration, in one of three formats (seconds, m:ss, h:mm:ss)"
  (disjunction (string)
   ;; Seconds format (a single integer):
    (prog1 (num in) (eof in))
    ;; mm:ss format (seconds must be modulo 60):
    (+ (* 60 (prog1 (num in) (colon in)))
        (prog1 (mod60 in) (eof in)))
    ;; h:mm:ss format (minutes and seconds must be modulo 60):
    (+ (* 3600 (prog1 (num in) (colon in)))
       (* 60 (prog1 (mod60 in) (colon in)))
       (prog1 (mod60 in) (eof in)))))

(defun do-seek (args)
  (let ((current *current-stream*)
        (time (and args (parse-timespec args))))
    (cond 
      ((null current) (format t "No song is playing.~%"))
      ((null time) (format t "Seek to where?~%"))
      (time (streamer-seek current *mixer* (* (mixer-rate *mixer*) time)))
      (t nil))))

(defun 12hour (in) (let ((n (num in))) (val (and (< 0 n 13) (mod n 12)))))

(defun parse-12-hour-format (in)
  "Parse numeric portions (hour or h:mm) of 12-hour time format, returning a count in minutes."
  (val
   (disjunction ()
     ;; h:mm format
     (+ (prog1 (* 60 (12hour in)) (colon in))
        (mod60 in))
     ;; Bare time in hours:
     (* 60 (12hour in)))))

(defun parse-daytime (in)
  "Parse string as a time of day (AM/PM), for the alarm
clock. Returns time in minutes from midnight."
  (disjunction ()
    ;; If there's no time, default to AM
    (prog1 (parse-12-hour-format in) (eof in))
    ;; AM time
    (prog1 (parse-12-hour-format in)
      (whitespace in)
      (disjunction () (match in "a.m.") (match in "am.") (match in "am"))
      (eof in))
    ;; PM time
    (+ (prog1 (parse-12-hour-format in)
         (whitespace in)
         (disjunction () (match in "p.m.") (match in "pm.") (match in "pm"))
         (eof in))
       720)))

(defun utime->string (utime)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time utime)
    (declare (ignore second))
    (format nil "~A ~A ~D ~D:~2,'0D ~A ~D"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            (nth month '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Nov" "Dec"))
            date
            (1+ (mod (1- hour) 12))
             minute
             (if (>= hour 12) "PM" "AM")
             year)))

(defun print-time ()
  (format t "~A~%" (utime->string (get-universal-time))))

(defun daytime->alarm-time (daytime)
  "Translate a daytime (in minutes) to a universal time for the
alarm. Since the daytime doesn't specify a date, we choose tomorrow
rather than today if the date would be less than the current time."
  (let* ((current-time (get-universal-time))
         (decoded (multiple-value-list (decode-universal-time current-time)))
         (minutes (second decoded))
         (hours   (third decoded))
         (current (+ minutes (* hours 60))))
    (cond 
      ((< current daytime)
       (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                              (fourth decoded) (fifth decoded) (sixth decoded)))
      (t (multiple-value-bind (s m h date month year) ; Get tomorrow's date.
             (decode-universal-time (+ current-time 86400))
           (declare (ignore s m h))
           (encode-universal-time 0 (mod daytime 60) (truncate daytime 60)
                                  date month year))))))

(defun parse-relative-time (in)
  (disjunction ()
    ;; Time in minutes:
    (prog1 (* 60 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "minutes") (match in "minute")
             (match in "mins") (match in "min") (match in "m")))
      (eof in))
    ;; Time in hours:
    (prog1 (* 3600 (num in))
      (whitespace in)
      (val (disjunction ()
             (match in "hours") (match in "hour")
             (match in "hr") (match in "h"))))
    ;; Time in h:mm format:
    (+ (* 3600 (prog1 (num in) (colon in)))
       (*   60 (mod60 in)))))

(defun parse-alarm-args (args)
  "Parse the arguments to the alarm command, returning NIL or a universal time."
  (disjunction (args)
    ;; State the time directly:
    (daytime->alarm-time (val (parse-daytime in)))
    ;; Syntactic sugar for stated time:
    (and (match in "at ") 
         (whitespace in)
         (daytime->alarm-time (val (parse-daytime in))))
    ;; Relative time offset:
    (and (match in "in ") 
         (whitespace in)
         (+ (get-universal-time) (val (parse-relative-time in))))))

(defvar *alarm-thread* nil)

(defun random-song ()
  (aref *library* (random (length *library*))))

(defun trigger-alarm ()
  ;; When the alarm goes off, unpause the player if it's paused. If it
  ;; isn't paused but there are songs in the queue, play the next
  ;; song. If the queue is empty, queue up ten random songs and play
  ;; one.
  (setf *wakeup-time* nil)
  (unless (unpause)
    (with-playqueue ()
      (unless *playqueue*
        (loop repeat 10 do (push (random-song) *playqueue*))))
    (play-next-song)))

(defun alarm-thread-toplevel ()
  (unwind-protect
       (loop as wakeup = *wakeup-time*
             as remaining = (and wakeup (- wakeup (get-universal-time))) 
             do (cond
                  ((null wakeup) (sleep 60))
                  ((<= remaining 0) (trigger-alarm))
                  (t (sleep (min remaining 60)))))
    (setf *alarm-thread* nil)))

(defun set-alarm (utime)
  (setf *wakeup-time* utime)
  (unless *alarm-thread*
    (setf *alarm-thread* (bordeaux-threads:make-thread #'alarm-thread-toplevel))))

(defun do-set-alarm (args) 
  (cond
    ((null args)
     (let ((wakeup *wakeup-time*))
       (if wakeup
           (format t "Alarm set for ~A~%" (utime->string wakeup))
           (format t "The alarm is not set.~%"))))
    ((member args '("off" "never" "delete" "disable" "cancel" "clear" "reset") :test #'string-equal)
     (setf *wakeup-time* nil)
     (format t "Disabled alarm.~%"))
    (t (let ((time (parse-alarm-args args)))
         (cond 
           ((null time) (format t "Unable to parse as time: ~W~%" args))
           (t (set-alarm time)
              (format t "Alarm set for ~A~%" (utime->string time))))))))

(defun print-help ()
  (format t "
Shuffletron is a text-mode music player oriented around search. Its principle
of operation is simple: search for songs, then play them. Searches are 
performed by typing a / followed by the search string:

library> /chromeo
       0  Electro/Chromeo_She_s_In_Control_10_Ah_Oui_Comme_Ca.mp3
       1  Electro/Chromeo_She_s_In_Control_1_My_And_My_Man.mp3
       2  Electro/Chromeo_She_s_In_Control_2_Needy_Girl.mp3
       3  Electro/Chromeo_She_s_In_Control_3_You_re_So_Gangsta.mp3
       4  Electro/Chromeo_She_s_In_Control_4_Woman_Friend.mp3
       5  Electro/Chromeo_She_s_In_Control_7_Since_You_Were_Gone.mp3
       6  Electro/Chromeo_She_s_In_Control_8_Way_Too_Much.mp3
       7  Electro/Chromeo_She_s_In_Control_9_Mercury_Tears.mp3
       8  Electro/DJ_Mehdi_I_Am_Somebody_featuring_Chromeo_2_I_Am_...

Each search result has a number to its left. This number allows you to
choose songs to play. Here, I decide to play song 8 then 0-3, in that
order:

9 matches> 8, 0-3

Any currently playing song is interrupted, and the chosen songs are
added to the head of the playback queue. To see the contents of the queue,
use the 'queue' command:

9 matches> queue
     (0)  Electro/Chromeo_She_s_In_Control_10_Ah_Oui_Comme_Ca.mp3
     (1)  Electro/Chromeo_She_s_In_Control_1_My_And_My_Man.mp3
     (2)  Electro/Chromeo_She_s_In_Control_2_Needy_Girl.mp3
     (3)  Electro/Chromeo_She_s_In_Control_3_You_re_So_Gangsta.mp3

  Now Playing: Electro/DJ_Mehdi_I_Am_Somebody_featuring_Chromeo_2_I_Am...

Notice that the prompt changed from \"library>\" to \"\9 matches>\"
after our search. Successive searches refine the result of previous
searches, and this indicates the number of items you're currently
searching within. If there had been more than 50 matches, they would
not be printed by default, but you could use the 'show' command at any
time to print them. Also note that the 'queue' command doesn't disrupt
the current search results (this is why numbering in the queue listing
is surrounded with parentheses, to indicate that entering numbers for
playback does not refer to them). The queue can be cleared with the
'clear' command, and the 'skip' command skips the current song and
advances to the next song in the queue.

To add songs to the queue without interrupting the current song, prefix the
song list with \"+\" (to append) or \"pre\" (to prepend).

When you've completed a search, a single blank line resets the results, and
the \"library>\" prompt will be restored.

Currently, only filenames are searched (not ID3 tags).

Additional help topics:
   help commands
   help examples
   help looping
   help alarms

"))

(defun print-commands ()
  (format t "
Command list:

  /[query]      Search library for [query].
  show          Print search matches. Songs in queue are marked with an asterisk.
  back          Undo last search.
  [songs]       Play list of songs.
  +[songs]      Append list of songs to queue.
  pre[songs]    Prepend list of songs to queue.
  queue         Print queue contents and current song playing.
  shuffle       Randomize order of songs in queue.
  clear         Clear the queue (current song continues playing)
  loop          Toggle loop mode (loop through songs in queue)

  now           Print name of song currently playing.
  pause         Toggle paused/unpaused.
  skip          Skip currently playing song.
  repeat [N]    Add N repetitions of currently playing song to head of queue.
  seek [time]   Seek to time (in [h:]m:ss format, or a number in seconds)

  time          Print current time
  alarm         Set alarm.
  
  exit          Exit the program.
  rescan        Rescan library directory for new additions.

  help [topic]  Help
"))

(defun print-examples ()
  (format t "
How to find and play a song, then return to library mode:

library> /vampire sushi
       0  WTF/Old Time Relijun/Witchcraft Rebellion/Old_Time_Relijun_Witchcraft_Rebellion_3_Vampire_Sushi.mp3
1 matches> 0
1 matches>
library>

How to refine search results:

library> /beatles
223 matches> /window
       0  Rock/The Beatles/Abbey Road/13 - She Came In Through The Bathroom Window.mp3
1 matches> 

How to play your entire library in shuffle mode:

  library> 0-            # Add open interval to queue
  library> shuffle       # Shuffle the queue
  library> skip          # It started playing before you shuffled, so skip..

"))

(defun print-loop-help ()
  (format t "
The \"loop\" command toggles looping mode. In looping mode, a song taken
from the head of the queue for playback is simultaneously added at the 
tail of the queue.

Choosing a single song to play (interrupting the current song) does not
affect the contents of the queue, and there's no issue in interrupting
a song which you'd like to continue looping in the queue, because it has
already been rotated to the end of the queue. This behavior allows you to
audition songs for addition to the queue without disturbing its contents.
When you've found a song you'd like to add the queue, you can do it in the
usual fashion, using the \"+\" or \"pre\" commands, or by abusing the 
\"repeat\" command (this will add the current song to the head of the queue).

In looping mode, a song which plays to completion (and was not originally
started from the queue) is added to the end of the queue. This provides 
another way to extend the queue while auditioning songs - if you allow the
song to play to completion, presumably you want to add it to the queue.
"))

(defun print-alarm-help ()
  (format t "
The \"alarm\" command provides an alarm clock feature which will play music
when the scheduled wakeup time is reached. There is a single wakeup time, 
and when it is reached the wakeup time is cleared. When the alarm is
triggered, the music player will do one of the following:

  1) If playback is paused, unpause the player.
  2) Otherwise, prepend ten random songs to queue and play them.

With no argument, the \"alarm\" command prints the current wakeup time. An 
argument to the command specifies the wakeup time. This can be done in a
variety of formats:

  alarm at 7:45 am      # \"at\" is optional and doesn't change the meaning
  alarm 7:45 am
  alarm 9 pm
  alarm 7               # If AM/PM not specified, assumes AM
  alarm in 5 minutes    # Relative alarm times, in minutes or hours
  alarm in 10m          # minutes, minute, mins, min, , m are synonyms
  alarm in 7 hours      # hours, hour, hr, h are synonyms
  alarm in 8h
  alarm in 7:29         # h:mm format - seven hours, twenty-nine minutes
  alarm reset           # off/never/delete/disable/cancel/clear/reset

If the player is already playing when the alarm goes off, the song
already playing will be interrupted by the next song in the queue.
"))

(defun parse-and-execute (line)
 (let* ((sepidx (position #\Space line))
        (command (subseq line 0 sepidx))
        (args    (and sepidx (string-trim " " (subseq line sepidx)))))
  (cond
    ;; A blank input line resets the query set.
    ((zerop (length line)) (reset-query))

    ;; Input starting with a forward slash refines the current query.
    ((char= (aref line 0) #\/) (refine-query (subseq line 1)))

    ;; Show all matches
    ((string= line "show") (show-current-query))

    ;; Quit
    ((or (string= line "quit") (string= line "exit"))
     (sb-ext:quit))

    ;; Play songs now (first is played, subsequent are added to queue
    ((digit-char-p (aref line 0))
     (let ((selected (selection-songs line)))
       (when (> (length selected) 0) 
         (play-song (elt selected 0) :enqueue-on-completion *loop-mode*))
       (when (> (length selected) 1)
         (with-playqueue ()
           (setf *playqueue* (concatenate 'list (subseq selected 1) *playqueue*))))))

    ;; Append songs and end of playqueue
    ((and (> (length line) 1) (char= #\+ (aref line 0)))
     (with-playqueue ()
       (setf *playqueue* (concatenate 'list 
                                      *playqueue*
                                      (selection-songs (subseq line 1)))))
     (unless (current-song-playing) (play-next-song)))

    ;; Prepend songs to playqueue
    ((and (>= (length line) 4)
          (string= "pre" (subseq line 0 3))
          (or (digit-char-p (aref line 3))
              (char= #\Space (aref line 3))))
     (with-playqueue () 
       (setf *playqueue* (concatenate 'list 
                                      (selection-songs (subseq line 3))
                                      *playqueue*)))
     (unless (current-song-playing) (play-next-song)))

    ;; Skip current song
    ((or (string= line "next") (string= line "skip"))
     (play-next-song)
     (show-current-song))

    ;; Pause playback
    ((string= line "pause") (toggle-pause))

    ;; Seek
    ((string= command "seek") (do-seek args))

    ;; Stop
    ;; Play?

    ;; Show playqueue
    ((string= line "queue") (show-playqueue))

    ;; Show current song
    ((string= line "now") (show-current-song))

    ;; Clear the queue
    ((string= line "clear")
     (with-playqueue ()
       (setf *playqueue* nil)))

    ;; Randomize queue
    ((string= line "shuffle")
     (with-playqueue ()
       (setf *playqueue* (alexandria:shuffle *playqueue*))))

    ;; Repeat the current song
    ((= 6 (or (mismatch "repeat" line) 6))
     (let ((song (current-song-playing))
           (num (or (parse-integer (subseq line 6) :junk-allowed t) 1)))
       (cond
         ((< num 0) (format t "  A negative repeat count doesn't make sense.~%"))
         ((not song) (format t "  No song is playing!~%"))
         (song
          (format t "  Repeating ~D time~:P: ~A~%" num (song-local-path song))
          (with-playqueue ()
            (setf *playqueue* (nconc (make-list num :initial-element song)
                                     *playqueue*)))))))

    ;; Back (restore previous selection)
    ((string= line "back")
     (when *selection-history*
       (setf *selection* (pop *selection-history*))))

    ;; Rescan library
    ((string= line "rescan")
     (rescan-library))

    ((string= line "loop")
     (setf *loop-mode* (not *loop-mode*))
     (format t "~&Loop mode ~A~%" (if *loop-mode* "enabled" "disabled")))

    ;; Print current time
    ((string= line "time") (print-time))

    ;; Set alarm clock
    ((string= command "alarm")
     (do-set-alarm args))

    ;; Help
    ((string= line "help") (print-help))

    ;; Help: Commands
    ((and (string= command "help")
          (equalp args "commands")) (print-commands))

    ;; Help: Examples
    ((and (string= command "help") 
          (equalp args "examples")) (print-examples))

    ;; Help: Looping
    ((and (string= command "help") 
          (equalp args "looping")) (print-loop-help))

    ;; Help: Alarms
    ((and (string= command "help") 
          (equalp args "alarms")) (print-alarm-help))

    ((string= command "help")
     (format t "Unknown help topic ~W~%" args))

    ;; Attempt to start swank server, for debugging.
    ((string= line "swankme") 
     (asdf:oos 'asdf:load-op :swank)     
     (eval (read-from-string "(swank:create-server :port 0)")))

    ;; Pager(?)
    ;; Tags
    ;; ???
    (t (format t "Unknown command ~W. Try 'help'.~%" line)))))

(defun mainloop ()
  (loop
   ;; Show the current query, if there aren't too many items:
   (when (and *selection-changed* (<= (length *selection*) *max-query-results*))
     (show-current-query))
   (setf *selection-changed* nil)
   ;; Prompt
   (format t "~A> " (if (querying-library-p) 
                        "library" 
                        (format nil "~:D matches" (length *selection*))))
   (force-output)
   ;; Input
   (let ((line (getline)))
     (flet ((cmd () (parse-and-execute (string-trim " " line))))
       (if *debug-mode*
           (cmd)
           (handler-case (cmd)
             (error (c) (format t "~&Oops! ~A~%" c))))))))

(defun ui-main ()
  (spooky-init)
  (audio-init)
  (init)
  (mainloop))

(ui-main)
