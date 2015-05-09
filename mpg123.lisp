;;;; CFFI bindings to libmpg123

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

(defpackage :mpg123
  (:use :common-lisp :cffi :mixalot-ffi-common :mixalot-strings-common)
  (:export
           #:mpg123-error
           #:mpg123-plain-error
           #:mpg123-handle-error
           #:check-mpg123-plain-error
           #:check-mh-error
           #:mpg123-strerror
           #:mpg123-plain-strerror

           #:MPG123_DONE
           #:MPG123_NEW_FORMAT
           #:MPG123_NEED_MORE
           #:MPG123_ERR
           #:MPG123_OK
           #:MPG123_BAD_OUTFORMAT
           #:MPG123_BAD_CHANNEL
           #:MPG123_BAD_RATE
           #:MPG123_ERR_16TO8TABLE
           #:MPG123_BAD_PARAM
           #:MPG123_BAD_BUFFER
           #:MPG123_OUT_OF_MEM
           #:MPG123_NOT_INITIALIZED
           #:MPG123_BAD_DECODER
           #:MPG123_BAD_HANDLE
           #:MPG123_NO_BUFFERS
           #:MPG123_BAD_RVA
           #:MPG123_NO_GAPLESS
           #:MPG123_NO_SPACE
           #:MPG123_BAD_TYPES
           #:MPG123_BAD_BAND
           #:MPG123_ERR_NULL
           #:MPG123_ERR_READER
           #:MPG123_NO_SEEK_FROM_END
           #:MPG123_BAD_WHENCE
           #:MPG123_NO_TIMEOUT
           #:MPG123_BAD_FILE
           #:MPG123_NO_SEEK
           #:MPG123_NO_READER
           #:MPG123_BAD_PARS
           #:MPG123_BAD_INDEX_PAR
           #:MPG123_OUT_OF_SYNC
           #:MPG123_RESYNC_FAIL

           #:mpg123-new
           #:mpg123-delete
           #:MPG123_FORCE_MONO
           #:MPG123_FORCE_STEREO
           #:MPG123_FORCE_8BIT
           #:MPG123_MONO_LEFT
           #:MPG123_MONO_RIGHT
           #:MPG123_MONO_MIX
           #:MPG123_QUIET
           #:MPG123_GAPLESS
           #:MPG123_NO_RESYNC
           #:MPG123_SEEKBUFFER
           #:mpg123-param
           #:mpg123-getparam
           #:mpg123-decoders
           #:mpg123-supported-decoders
           #:mpg123-decoder
           #:MPG123_ENC_16
           #:MPG123_ENC_SIGNED
           #:MPG123_ENC_8
           #:MPG123_ENC_SIGNED_16
           #:MPG123_ENC_UNSIGNED_16
           #:MPG123_ENC_UNSIGNED_8
           #:MPG123_ENC_SIGNED_8
           #:MPG123_ENC_ULAW_8
           #:MPG123_ENC_ALAW_8
           #:MPG123_ENC_ANY
           #:mpg123-format-none
           #:mpg123-format-all
           #:mpg123-format
           #:mpg123-format-support
           #:%mpg123-getformat
           #:mpg123-getformat
           #:mpg123-open
           #:mpg123-open-fd
           #:mpg123-open-feed
           #:mpg123-close
           #:mpg123-read
           #:mpg123-decode
           #:mpg123-tell
           #:mpg123-tellframe
           #:mpg123-seek
           #:MPG123_CRC
           #:MPG123_COPYRIGHT
           #:MPG123_PRIVATE
           #:MPG123_ORIGINAL
           #:frameinfo
           #:frameinfo-mpeg-version
           #:frameinfo-layer
           #:frameinfo-rate-hz
           #:frameinfo-mode
           #:frameinfo-mode-ext
           #:frameinfo-framesize
           #:frameinfo-flags
           #:frameinfo-emphasis
           #:frameinfo-bitrate
           #:frameinfo-abr-rate
           #:frameinfo-vbr
           #:mpg123-info
           #:mpg123-scan
           #:mpg123-length

           #:mpg123-tpf
           #:mpg123-clip
           #:mpg123-string
           #:mpg123-string-data
           #:mpg123-string-size
           #:mpg123-string-fill
           #:mpg123-init-string
           #:mpg123-free-string
           #:mpg123-resize-string
           #:mpg123-copy-string
           #:mpg123-add-string
           #:mpg123-set-string
           #:mpg123-text
           ;; FIXME: Text accessors.
           #:mpg123-id3v2
           #:id3v2-title
           #:id3v2-artist
           #:id3v2-album
           #:id3v2-year
           #:id3v2-genre
           #:id3v2-comment
           #:id3v2-comment-list
           #:id3v2-comments
           #:id3v2-text
           #:id3v2-texts
           #:id3v2-extra
           #:id3v2-extras
           #:mpg123-id3v1
           #:MPG123_ID3
           #:MPG123_NEW_ID3
           #:MPG123_ICY
           #:MPG123_NEW_ICY
           #:mpg123-meta-check
           #:mpg123-id3
           #:mpg123-icy

           #:ensure-libmpg123-initialized

           #:get-bitstream-properties
           #:get-tags-from-handle
           #:get-tags-from-file
           #:decode-mp3-file))

(in-package :mpg123)


(define-foreign-library libmpg123
  (:darwin (:or "libmpg123.dylib" "libmpg123.0.dylib"
                "/opt/local/lib/libmpg123.0.dylib"))
  (:unix (:or "libmixalot-mpg123.so.0"
              "libmpg123.so.0"
              "/usr/lib/libmpg123.so"
              "/usr/local/lib/libmpg123.so"))
  (t (:default "libmpg123")))

(use-foreign-library libmpg123)

;;;; Basic types

(defctype handleptr :pointer)

;;;; Error handling

(define-condition mpg123-error (error) ())

(define-condition mpg123-plain-error (mpg123-error)
  ((text :initarg :text))
  (:documentation "An error from the mpg123 library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defcfun mpg123-plain-strerror :string (errcode :int))

(defun check-mpg123-plain-error (circumstance result)
  (unless (zerop result)
    (error 'mpg123-plain-error
           :text (format nil "~A: ~A" circumstance
                         (mpg123-plain-strerror result)))))

(define-condition mpg123-handle-error (mpg123-error)
  ((text :initarg :text)
   (handle :initarg :handle))
  (:documentation "An error from the mpg123 library")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text) stream))))

(defcfun mpg123-strerror :string
  (mh handleptr))

(defun check-mh-error (circumstance handle return-value)
  (if (>= return-value 0)
      return-value
      (error 'mpg123-handle-error
             :handle handle
             :text (format nil "~A on handle ~A: ~A" circumstance handle
                           (mpg123-strerror handle)))))

;;;; Error codes

(defconstant MPG123_DONE -12)           ; Message: Track ended.
(defconstant MPG123_NEW_FORMAT -11)     ; Message: Output format will be different on next call.
(defconstant MPG123_NEED_MORE -10)      ; Message: For feed reader: "Feed me more!"
(defconstant MPG123_ERR -1)             ; Generic Error
(defconstant MPG123_OK 0)               ; Success
(defconstant MPG123_BAD_OUTFORMAT 1)    ; Unable to set up output format!
(defconstant MPG123_BAD_CHANNEL 2)      ; Invalid channel number specified.
(defconstant MPG123_BAD_RATE 3)         ; Invalid sample rate specified.
(defconstant MPG123_ERR_16TO8TABLE 4)   ; Unable to allocate memory for 16 to 8 converter table!
(defconstant MPG123_BAD_PARAM 5)        ; Bad parameter id!
(defconstant MPG123_BAD_BUFFER 6)       ; Bad buffer given -- invalid pointer or too small size.
(defconstant MPG123_OUT_OF_MEM 7)       ; Out of memory -- some malloc() failed.
(defconstant MPG123_NOT_INITIALIZED 8)  ; You didn't initialize the library!
(defconstant MPG123_BAD_DECODER 9)      ; Invalid decoder choice.
(defconstant MPG123_BAD_HANDLE 10)      ; Invalid mpg123 handle.
(defconstant MPG123_NO_BUFFERS 11)      ; Unable to initialize frame buffers (out of memory?).
(defconstant MPG123_BAD_RVA 12)         ; Invalid RVA mode.
(defconstant MPG123_NO_GAPLESS 13)      ; This build doesn't support gapless decoding.
(defconstant MPG123_NO_SPACE 14)        ; Not enough buffer space.
(defconstant MPG123_BAD_TYPES 15)       ; Incompatible numeric data types.
(defconstant MPG123_BAD_BAND 16)        ; Bad equalizer band.
(defconstant MPG123_ERR_NULL 17)        ; Null pointer given where valid storage address needed.
(defconstant MPG123_ERR_READER 18)      ; Error reading the stream.
(defconstant MPG123_NO_SEEK_FROM_END 19) ; Cannot seek from end (end is not known).
(defconstant MPG123_BAD_WHENCE 20)      ; Invalid 'whence' for seek function.
(defconstant MPG123_NO_TIMEOUT 21)      ; Build does not support stream timeouts.
(defconstant MPG123_BAD_FILE 22)        ; File access error.
(defconstant MPG123_NO_SEEK 23)         ; Seek not supported by stream.
(defconstant MPG123_NO_READER 24)       ; No stream opened.
(defconstant MPG123_BAD_PARS 25)        ; Bad parameter handle.
(defconstant MPG123_BAD_INDEX_PAR 26)   ; Bad parameters to mpg123_index()
(defconstant MPG123_OUT_OF_SYNC 27)     ; Lost track in bytestream and did not try to resync.
(defconstant MPG123_RESYNC_FAIL 28)     ; Resync failed to find valid MPEG data.


;;;; Interface to interesting bits of mpg123 library.

;;; Initialize the mpg123 library. Not thread safe. Call exactly once,
;;; before any other calls to the library.
(defcfun mpg123-init :int)

;;; Close down the mpg123 library. Also not thread safe.
;;; Don't call this if you're using ensure-libmpg123-initialized,
;;; because it won't realize.
(defcfun mpg123-exit :void)

;;; Create a handle using the chosen decoder. You can pass a null
;;; pointer for the decode name.
(defcfun mpg123-new handleptr
  (decoder :string)
  (error (:pointer :int)))

;;; Delete a handle.
(defcfun mpg123-delete :void
  (mh handleptr))

(defcenum mpg123-parms
  :VERBOSE         ; set verbosity value for enabling messages to stderr, >= 0 makes sense
  :FLAGS           ; set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX
  :ADD-FLAGS       ; add some flags
  :FORCE-RATE      ; when value > 0, force output rate to that value
  :DOWN-SAMPLE     ; 0=native rate, 1=half rate, 2=quarter rate
  :RVA             ; one of the RVA choices above
  :DOWNSPEED       ; play a frame N times
  :UPSPEED         ; play every Nth frame
  :START-FRAME     ; start with this frame (skip frames before that)
  :DECODE-FRAMES   ; decode only this number of frames
  :ICY-INTERVAL    ; stream contains ICY metadata with this interval
  :OUTSCALE        ; the scale for output samples (amplitude)
  :TIMEOUT         ; timeout for reading from a stream (not supported on win32)
  :REMOVE-FLAGS    ; remove some flags (inverse of MPG123_ADD_FLAGS)
  :RESYNC-LIMIT)   ; Try resync on frame parsing for that many bytes or until end of stream (<0).

(defconstant MPG123_FORCE_MONO     #x7)    ;     0111 Force some mono mode: This is a test bitmask for seeing if any mono forcing is active.
(defconstant MPG123_MONO_LEFT      #x1)    ;     0001 Force playback of left channel only.
(defconstant MPG123_MONO_RIGHT     #x2)    ;     0010 Force playback of right channel only.
(defconstant MPG123_MONO_MIX       #x4)    ;     0100 Force playback of mixed mono.
(defconstant MPG123_FORCE_STEREO   #x8)    ;     1000 Force stereo output.
(defconstant MPG123_FORCE_8BIT     #x10)   ; 00010000 Force 8bit formats.
(defconstant MPG123_QUIET          #x20)   ; 00100000 Suppress any printouts (overrules verbose).
(defconstant MPG123_GAPLESS        #x40)   ; 01000000 Enable gapless decoding (default on if libmpg123 has support).
(defconstant MPG123_NO_RESYNC      #x80)   ; 10000000 Disable resync stream after error.
(defconstant MPG123_SEEKBUFFER     #x100)  ; 000100000000 Enable small buffer on non-seekable streams to allow some peek-ahead (for better MPEG sync).

(defcfun mpg123-param :int
  (mh      handleptr)
  (type    mpg123-parms)
  (valptr  :long)
  (fvalptr :double))

(defcfun mpg123-getparam :int
  (mh      handleptr)
  (type    mpg123-parms)
  (valptr  (:pointer :long))
  (fvalptr (:pointer :double)))

;;; Decoders query:
(defcfun (%mpg123-decoders "mpg123_decoders") :pointer)
(defcfun (%mpg123-supported-decoders "mpg123_supported_decoders") :pointer)

(defun translate-string-pointers (pointers)
  (loop for i upfrom 0 below 5
        as ptr = (mem-ref pointers :pointer (* i (foreign-type-size :pointer)))
        until (null-pointer-p ptr)
        collect (foreign-string-to-lisp ptr)))

(defun mpg123-decoders ()
  (translate-string-pointers (%mpg123-decoders)))

(defun mpg123-supported-decoders ()
  (translate-string-pointers (%mpg123-supported-decoders)))

;;; Select a decoder by name.
(defcfun mpg123-decoder :int
  (mh           handleptr)
  (decoder-name :string))

;;; Output encodings
(defconstant MPG123_ENC_16      #x40)                                                 ; 0100 0000 Some 16 bit encoding...
(defconstant MPG123_ENC_SIGNED  #x80)                                                 ; 1000 0000 Some signed encoding...
(defconstant MPG123_ENC_8       #x0f)                                                 ; 0000 1111 Some 8 bit encoding...
(defconstant MPG123_ENC_SIGNED_16    (logior MPG123_ENC_16 MPG123_ENC_SIGNED #x10))   ; 1101 0000 signed 16 bit
(defconstant MPG123_ENC_UNSIGNED_16  (logior MPG123_ENC_16 #x20))                     ; 0110 0000 unsigned 16 bit
(defconstant MPG123_ENC_UNSIGNED_8   #x01)                                            ; 0000 0001 unsigned 8 bit
(defconstant MPG123_ENC_SIGNED_8     (logior MPG123_ENC_SIGNED #x02))                 ; 1000 0010 signed 8 bit
(defconstant MPG123_ENC_ULAW_8       #x04)                                            ; 0000 0100 ulaw 8 bit
(defconstant MPG123_ENC_ALAW_8       #x08)                                            ; 0000 1000 alaw 8 bit
(defconstant MPG123_ENC_ANY
  (logior MPG123_ENC_SIGNED_16 MPG123_ENC_UNSIGNED_16
          MPG123_ENC_UNSIGNED_8 MPG123_ENC_SIGNED_8
          MPG123_ENC_ULAW_8 MPG123_ENC_ALAW_8))

;;; Configure handle to accept no output format (use before specifying
;;; your desired format).
(defcfun mpg123-format-none :int
  (mh handleptr))

;;; Configure handle to accept any output format (default).
(defcfun mpg123-format-all :int
  (mh handleptr))

;;; Add a specific output format to handle.
(defcfun mpg123-format :int
  (mh        handleptr)
  (rate      :long)
  (channels  :int)
  (encodings :int))

;;; Check if a specific format/rate is supported by the handle (returns 0 if supported).
(defcfun mpg123-format-support :int
  (mh       handleptr)
  (rate     :long)
  (encoding :int))

(defcfun (%mpg123-getformat "mpg123_getformat") :int
  (mh           handleptr)
  (rate-out     (:pointer :long))
  (channels-out (:pointer :int))
  (encoding-out (:pointer :int)))

(defun mpg123-getformat (mh)
  (with-foreign-objects ((rate :long)
                         (channels :int)
                         (encoding :int))
    (check-mh-error "mpg123 get format"  mh
     (%mpg123-getformat mh rate channels encoding))
    (values (mem-ref rate :long)
            (mem-ref channels :int)
            (mem-ref encoding :int))))

;;; Opening/closing bitstreams:

;;; Open and prepare to decode a local file.
(defcfun mpg123-open :int
  (mh   handleptr)
  (path :string))

;;; Use an existing file descriptor as the bitstream input.
(defcfun mpg123-open-fd :int
  (mh   handleptr)
  (fd   :int))

;;; Open new bitstream for direct feeding using mpg123-decode.
(defcfun mpg123-open-feed :int
  (mh   handleptr))

;;; Closes source, if it was opened by libmpg123.
(defcfun mpg123-close :int
  (mh   handleptr))

;;; Read from stream and decode up to 'maxlength' bytes into
;;; 'outmem'. Number of bytes actually decoded is stored to *done.
;;; Mind the return value (MPG123_DONE, etc.)
(defcfun mpg123-read :int
  (mh        handleptr)
  (outmem    (:pointer :uchar))
  (maxlength size_t)
  (done      (:pointer size_t)))

;;; Decode audio from inbuf to outbuf. A zero length output buffer
;;; will allow you to detect a change in the output format (with a
;;; return value of MPG123_NEW_FORMAT) without taking decoded data.
;;; Number of bytes actually decoded is written out through *done.
;;; Mind the return value (MPG123_NEED_MORE, etc.)
(defcfun mpg123-decode :int
  (mh handleptr)
  (inmem  (:pointer :uchar))
  (insize size_t)
  (outmem (:pointer :uchar))
  (outsize size_t)
  (done   (:pointer size_t)))

;;; Position and seeking

;;; Returns the current position in samples.
(defcfun mpg123-tell off_t
  (mh handleptr))

;;; Returns the frame number which the next read will return samples from.
(defcfun mpg123-tellframe off_t
  (mh handleptr))

(defcenum whence
  (:set 0)
  (:cur 1)
  (:end 2))

;;; Seek to a desired sample offset.
(defcfun mpg123-seek off_t
  (mh handleptr)
  (sample-offset off_t)
  (whence whence))

;;; Skipped various similar functions that mostly looked of interest
;;; for direct feeding mode.

(defcenum mpg123-vbr
  (:cbr 0) :vbr :abr)

(defcenum mpg123-version
  (:v1.0 0) :v2.0 :v2.5)

(defcenum mpg123-mode
  (:stereo 0) :joint :dual :single)

(defconstant MPG123_CRC       #x1)
(defconstant MPG123_COPYRIGHT #x2)
(defconstant MPG123_PRIVATE   #x4)
(defconstant MPG123_ORIGINAL  #x8)

(defcstruct (frameinfo :conc-name frameinfo-)
  (mpeg-version mpg123-version)
  (layer     :int)
  (rate-hz   :long)
  (mode      mpg123-mode)
  (mode-ext  :int)
  (framesize :int)
  (flags     :uint)                     ; Stupid abuse of enum in the C code.
  (emphasis  :int)
  (bitrate   :int)
  (abr-rate  :int)
  (vbr       mpg123-vbr))

;;; Store information about MPEG audio bitstream into frameinfo structure.

;;; * Note that this expects the decoder is already initialized, by
;;; e.g. decoding some audio, or calling mpg123_getformat. Unlike many
;;; libmpg123 functions, it won't take care of this itself. Instead,
;;; it tries to decode uninitialized data in the handle structure and
;;; will likely segfault.

(defcfun mpg123-info :int
  (mh handleptr)
  (mi (:pointer (:struct frameinfo))))

;;; Scan each frame in the file, locating ID3 tags and computing an
;;; accurate length.
(defcfun mpg123-scan :int
  (mh handleptr))

;;; Return the full (expected) length in samples of the current track,
;;; if it can be determined.
(defcfun mpg123-length off_t
  (mh handleptr))

;;; Return the time per frame (in seconds)
(defcfun mpg123-tpf :double
  (mh handleptr))

;;; Get and reset the clip count.
(defcfun mpg123-clip :long
  (mh handleptr))

;;;; MPG123 strings library

(defcstruct (mpg123-string :conc-name mpg123-string-)
  (data (:pointer :char))
  (size size_t)
  (fill size_t))

;;; Create and allocate memory for a new mpg123 string.
(defcfun mpg123-init-string :void
  (sb (:pointer (:struct mpg123-string))))

;;; Free memory for an mpg123-string
(defcfun mpg123-free-string :void
  (sb (:pointer (:struct mpg123-string))))

;;; Resize mpg123 string. Returns 1 on success, 0 on error.
(defcfun mpg123-resize-string :int
  (sb (:pointer (:struct mpg123-string)))
  (new-size size_t))

;;; Copy an mpg123-string. Returns 1 on success, 0 on error.
(defcfun mpg123-copy-string :int
  (from (:pointer (:struct mpg123-string)))
  (to   (:pointer (:struct mpg123-string))))

;;; Append a C (or Lisp) string to an mpg123 string.
;;; Returns 1 on success, 0 on error.
(defcfun mpg123-add-string :int
  (stem (:pointer (:struct mpg123-string)))
  (tail :string))

;;; Assign a C (or Lisp) string to an mpg123 string.
;;; Returns 1 on success, 0 on error.
(defcfun mpg123-set-string :int
  (sb (:pointer (:struct mpg123-string)))
  (new-contents :string))

;;;; ID3 library

;;; To work around a CFFI bug, we can't generate the accessors using :conc-name.
(defcstruct mpg123-text ;; (mpg123-text :conc-name mpg123-text-)
  (lang :char :count 3)   ; Three-letter language code.
  (id   :char :count 4)   ; IDv2 text field ID
  (description (:struct mpg123-string))
  (text        (:struct mpg123-string)))

(defcstruct (mpg123-id3v2 :conc-name id3v2-)
  (version :uchar)
  (title   (:pointer (:struct mpg123-string)))
  (artist  (:pointer (:struct mpg123-string)))
  (album   (:pointer (:struct mpg123-string)))
  (year    (:pointer (:struct mpg123-string)))
  (genre   (:pointer (:struct mpg123-string)))
  (comment (:pointer (:struct mpg123-string)))
  (comment-list (:pointer (:struct mpg123-text)))
  (comments     size_t)
  (text         (:pointer (:struct mpg123-text)))
  (texts        size_t)
  (extra        (:pointer (:struct mpg123-text)))
  (extras       size_t))

;; FIXME: CFFI bug.
(defcstruct (mpg123-id3v1 #|:conc-name id3v1-|#)
  (tag     :char :count 3)
  (title   :char :count 30)
  (artist  :char :count 30)
  (album   :char :count 30)
  (year    :char :count 4)
  (comment :char :count 30)
  (genre   :uchar))

;;; Metadata presence bits:

(defconstant MPG123_ID3     #x3)  ; 0011 There is some ID3 info. Also matches 0010 or NEW_ID3.
(defconstant MPG123_NEW_ID3 #x1)  ; 0001 There is ID3 info that changed since last call to mpg123_id3.
(defconstant MPG123_ICY     #xC)  ; 1100 There is some ICY info. Also matches 0100 or NEW_ICY.
(defconstant MPG123_NEW_ICY #x4)  ; 0100 There is ICY info that changed since last call to mpg123_icy.

;;; Query if there is new metadata (ID3 or ICY), returning a
;;; combination of the above flags.
(defcfun mpg123-meta-check :int
  (mh handleptr))

;;; Query ID3 tags. Pointers written out through v1/v2 will be NULL or
;;; reference a volatile internal buffer containing tag info which may
;;; be changed by the next read/decode call.
;;; Returns MPG123_OK or MPG123_ERR.
(defcfun mpg123-id3 :int
  (mh handleptr)
  (v1 (:pointer (:pointer (:struct mpg123-id3v1))))
  (v2 (:pointer (:pointer (:struct mpg123-id3v2)))))

;;; Query ICY data. icy-meta is pointed to an existing data structure
;;; which may change after the next read/decode call.
;;; Returns MPG123_OK or MPG123_ERR.
(defcfun mpg123-icy :int
  (mh handleptr)
  (icy-meta (:pointer (:pointer :char))))

;;; FFI to the advanced parameter API is not implemented, because I
;;; don't need it.


;;;; Wrapper around library initialization

(defvar *libmpg123-initialized* nil)

(defun ensure-libmpg123-initialized ()
  "Ensure libmpg123 is initialized. Not thread safe, although once
initialized the library can be used in a threaded fashion."
  (unless *libmpg123-initialized*
    (check-mpg123-plain-error "libmpg123 initialization" (mpg123-init))
    (setf *libmpg123-initialized* t))
  (values))


;;;; ID3 utilities

(defvar *id3-no-unicode* nil
  "If true, always convert id3 values as ISO-8859-1.")

#+NIL
(defun safely-convert-string (c-string-ptr encoding
                              &optional (max-length (1- array-total-size-limit)))
  "Safely convert a C string to a lisp string under the specified
encoding, returning NIL if the conversion can not be performed. If
successful, returns two values: the converted lisp string, and the
encoding."
  (handler-case (values (foreign-string-to-lisp c-string-ptr
                                                :encoding encoding
                                                :max-chars max-length)
                        encoding)
    (babel-encodings:character-coding-error ()
      nil)))

#+NIL
(defun magic-string-conversion (c-string-ptr)
  "Attempts to convert the pointer to a lisp string using UTF-8
  encoding. If that fails, converts it using ISO-8859-1. Returns two
  values: the converted lisp string, and the encoding used (one
  of :utf-8 or :iso-8859-1)."
  (let ((utf-8 (and (not *id3-no-unicode*)
                    (safely-convert-string c-string-ptr :utf-8))))
    (cond (utf-8 (values utf-8 :utf-8))
          (t (safely-convert-string c-string-ptr :iso-8859-1)))))

(defun nullify (sequence)
  (and (not (zerop (length sequence))) sequence))

;;; My policy: Prefer utf-8 for ID3v2 fields then fall back to
;;; iso-8859-1. Always convert ID3v1 fields as iso-8859-1.
;;; Except when id3-no-unicode* is on, because in practice,
;;; unicode strings just break everything.

(defun get-id3v2-field (v2 v2-accessor)
  (and (not (null-pointer-p v2))
       (let ((ptr (funcall v2-accessor v2)))
         (and (not (null-pointer-p ptr))
              (trim-if-string
               (magic-string-conversion (mpg123-string-data ptr)
                                        :no-utf8 *id3-no-unicode*))))))

(defun get-id3v1-field (v1 v1-slot-name v1-field-width)
  (and (not (null-pointer-p v1))
       (trim-if-string
        (safely-convert-string
         (foreign-slot-pointer v1 'mpg123-id3v1 v1-slot-name)
         :iso-8859-1 v1-field-width))))

(defun id3-field-best-value (v1 v2 v2-accessor v1-slot-name v1-field-width)
  (or (nullify (get-id3v2-field v2 v2-accessor))
      (nullify (get-id3v1-field v1 v1-slot-name v1-field-width))))

(defparameter *id3v1-genres*
  #("Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge" "Hip-Hop"
    "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap" "Reggae"
    "Rock" "Techno" "Industrial" "Alternative" "Ska" "Death Metal" "Pranks"
    "Soundtrack" "Euro-Techno" "Ambient" "Trip-Hop" "Vocal" "Jazz+Funk"
    "Fusion" "Trance" "Classical" "Instrumental" "Acid" "House" "Game"
    "Sound Clip" "Gospel" "Noise" "AlternRock" "Bass" "Soul" "Punk" "Space"
    "Meditative" "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic"
    "Darkwave" "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance"
    "Dream" "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave" "Psychadelic"
    "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal" "Acid Punk" "Acid Jazz"
    "Polka" "Retro" "Musical" "Rock & Roll" "Hard Rock" "Folk" "Folk-Rock"
    "National Folk" "Swing" "Fast Fusion" "Bebob" "Latin" "Revival" "Celtic"
    "Bluegrass" "Avantgarde" "Gothic Rock" "Progressive Rock"
    "Psychedelic Rock" "Symphonic Rock" "Slow Rock" "Big Band" "Chorus"
    "Easy Listening" "Acoustic" "Humour" "Speech" "Chanson" "Opera"
    "Chamber Music" "Sonata" "Symphony" "Booty Bass" "Primus" "Porn Groove"
    "Satire" "Slow Jam" "Club" "Tango" "Samba" "Folklore" "Ballad"
    "Power Ballad" "Rhythmic Soul" "Freestyle" "Duet" "Punk Rock" "Drum Solo"
    "A capella" "Euro-House" "Dance Hall"))

(defun translate-id3v1-genre (integer)
  (and (< integer (length *id3v1-genres*))
       (aref *id3v1-genres* integer)))

(defun property (keyword value &optional plist)
  (if (null value)
      plist
      (list* keyword value plist)))

(defun get-genre (v1 v2)
  (let ((v2-genre (nullify (get-id3v2-field v2 'id3v2-genre)))
        (v1-genre (and (not (null-pointer-p v1))
                       (foreign-slot-value v1 '(:struct mpg123-id3v1) 'genre))))
    ;; Work around stupid numeric v2 genres that some program creates:
    (when (and (>= (length v2-genre) 3)
               (char= (aref v2-genre 0) #\()
               (digit-char-p (aref v2-genre 1)))
      (let ((n (parse-integer v2-genre :start 1 :junk-allowed t :radix 10)))
        (setf v2-genre (and n (translate-id3v1-genre n)))))
    ;; Return genre, preferring the version 2 string.
    (or v2-genre (and v1-genre (translate-id3v1-genre v1-genre)))))

(defun convert-mpg123-string (str)
  (let ((ptr (mpg123-string-data str)))
    (and (not (null-pointer-p ptr))
         (magic-string-conversion ptr :no-utf8 *id3-no-unicode*))))

(defun dump-mpg123-text (group n text)
  (format t "~& ~7<~A~> ~D: lang=~4A id=~4A description=~W text=~W~%"
          group n
          (safely-convert-string (foreign-slot-pointer text 'mpg123-text 'lang)
                                 :iso-8859-1 3)
          (safely-convert-string (foreign-slot-pointer text 'mpg123-text 'id)
                                 :iso-8859-1 4)
          (convert-mpg123-string (foreign-slot-pointer text 'mpg123-text 'description))
          (convert-mpg123-string (foreign-slot-pointer text 'mpg123-text 'text))))

(defun find-text-tag (array-ptr n id)
  (loop for i from 0 below n
        as text-ptr = (inc-pointer array-ptr (* i (foreign-type-size 'mpg123-text)))
        as this-id = (safely-convert-string
                      (foreign-slot-pointer text-ptr 'mpg123-text 'id) :iso-8859-1 4)
        when (equal id this-id)
        do (return
             (convert-mpg123-string (foreign-slot-pointer text-ptr 'mpg123-text 'text)))))

(defun dump-mpg123-texts (group array-ptr n)
  (dotimes (i n)
    (dump-mpg123-text group i
     (inc-pointer array-ptr
                  (* i (foreign-type-size 'mpg123-text))))))

(defun get-track (v1 v2)
  (declare (ignore v1))
  ;; TODO: Check for ID3v1.1 track number.
  (unless (null-pointer-p v2)
    (let ((trck (find-text-tag (id3v2-text v2) (id3v2-texts v2) "TRCK")))
      (and trck (parse-integer trck :junk-allowed t)))))

(defun fix-year (year)
  (case (length year)
    ;; Discard single digit year fields.
    (1 nil)
    ;; Map 3x..9x to 193x..199x and 0x to 200x. By 2010 I expect you
    ;; all to produce correct tags.
    (2 (or (and (find (aref year 0) "3456789")
                (concatenate 'string "19" year))
           (and (char= #\0 (aref year 0))
                (concatenate 'string "20" year))))
    ;; I'm feeling generous, so try to fix up busted 19xx years.
    (3 (and (char= (aref year 0) #\9)
            (concatenate 'string "1" year)))
    ;; Reject implausible years, with apologies to fans of pre-Baroque music.
    (4 (when (or (and (char= (aref year 0) #\1)
                      (find (aref year 1) "9876"))
                 (and (char= (aref year 0) #\2)
                      (char= (aref year 1) #\0)))
         year))))

(defun get-bitstream-properties (handle)
  (with-foreign-object (frameinfo '(:struct frameinfo))
    (check-mpg123-plain-error
     "Get frameinfo"
     (mpg123-info handle frameinfo))
    (list
     :mpeg-version
     (frameinfo-mpeg-version frameinfo)
     :layer
     (frameinfo-layer frameinfo)
     :rate-hz
     (frameinfo-rate-hz frameinfo)
     :mode
     (frameinfo-mode frameinfo)
     :mode-ext
     (frameinfo-mode-ext frameinfo)
     :flags
     (frameinfo-flags frameinfo)
     :emphasis
     (frameinfo-emphasis frameinfo)
     :bitrate
     (frameinfo-bitrate frameinfo)
     :abr-rate
     (frameinfo-abr-rate frameinfo)
     :vbr
     (frameinfo-vbr frameinfo))))

(defun get-tags-from-handle (handle &key print-misc-tags (no-utf8 nil))
"Parse ID3 from the given mpg123 handle and return some subset of
title, artist, album, year, comment, tack, and genre as a property
list."
  (with-foreign-objects ((v1p '(:pointer (:struct mpg123-id3v1)))
                         (v2p '(:pointer (:struct mpg123-id3v2))))
    (check-mh-error "Get ID3 tags" handle (mpg123-id3 handle v1p v2p))
    (let ((v1 (mem-ref v1p :pointer))
          (v2 (mem-ref v2p :pointer))
          (*id3-no-unicode* no-utf8))
      (loop for (keyword v2-accessor v1-slot v1-field-width) in
            '((:title   id3v2-title   title   30)
              (:artist  id3v2-artist  artist  30)
              (:album   id3v2-album   album   30)
              (:year    id3v2-year    year     4)
              (:comment id3v2-comment comment 30))
            as value = (id3-field-best-value v1 v2 v2-accessor v1-slot v1-field-width)
            when value
            nconcing (list keyword value) into properties
            finally
            ;; Cleanup obviously bogus tags.
            (let ((year (getf properties :year)))
              (setf year (and year (fix-year year)))
              (if year
                  (setf (getf properties :year) year)
                  (remf properties :year)))
            (setf properties (nconc properties
                                    (property :track (get-track v1 v2))
                                    (property :genre (get-genre v1 v2))))
            ;; No such thing as track 0. Some files have track in comment field.
            (when (prefix-p "Track " (getf properties :comment))
              (let ((tr (parse-integer (getf properties :comment) :start 6 :junk-allowed t)))
                (when (and tr (> tr 0) (< tr 100))
                  (remf properties :comment)
                  (setf (getf properties :track) tr))))
            ;; Debugging: Print all text entries, if enabled
            (when (and print-misc-tags (not (null-pointer-p v2)))
              (dump-mpg123-texts "Comment" (id3v2-comment-list v2) (id3v2-comments v2))
              (dump-mpg123-texts "Text"    (id3v2-text v2)         (id3v2-texts v2))
              (dump-mpg123-texts "Extra"   (id3v2-extra v2)        (id3v2-extras v2)))
            (return
              (values
               (clean-tags properties)
               (get-bitstream-properties handle)))))))

(defun get-tags-from-file (filename &key
                           verbose
                           (character-encoding :iso-8859-1)
                           (no-utf8 nil))
  "Parse ID3 tags of the given file and return some subset of title,
artist, album, year, comment, tack, and genre as a property list."
  (ensure-libmpg123-initialized)
  (with-foreign-object (err :int)
    (let ((handle (mpg123-new (null-pointer) err)))
      (check-mpg123-plain-error "mpg123-new" (mem-ref err :int))
      (unless verbose
        (mpg123-param handle :add-flags MPG123_QUIET 0.0d0))
      (with-foreign-string (unmangled filename :encoding character-encoding)
        (check-mh-error "mpg123 open file" handle (mpg123-open handle unmangled)))
      (unwind-protect
           (progn
             ;; This is necessary to initialize the decoder:
             (mpg123-getformat handle)
             ;; Now we can get the tags:
             (get-tags-from-handle handle :no-utf8 no-utf8))
        ;; Unwind cleanups:
        (mpg123-close handle)
        (mpg123-delete handle)))))

;;; Easy decoding of an MP3 file:

(defun decode-mp3-file (filename &key verbose (character-encoding :iso-8859-1))
  "Decode an mp3 file, returning four values: a vector containing the samples
   (in 16-bit format), and rate, channels, and encoding corresponding to the
   return values of mpg123-getformat."
  (ensure-libmpg123-initialized)
  (with-foreign-object (err :int)
    (let ((handle (mpg123-new (null-pointer) err)))
      (check-mpg123-plain-error "mpg123-new" (mem-ref err :int))
      (unless verbose
        (mpg123-param handle :add-flags MPG123_QUIET 0.0d0))
      (with-foreign-string (unmangled filename :encoding character-encoding)
        (check-mh-error "mpg123 open file" handle (mpg123-open handle unmangled)))
      (unwind-protect
           (multiple-value-bind (rate channels encoding) (mpg123-getformat handle)
             (mpg123-format-none handle)
             (mpg123-format handle rate 2 MPG123_ENC_UNSIGNED_16)
             (mpg123-scan handle)
             (let* ((length (* channels (mpg123-length handle)))
                    (buffer (make-array (* 2 length) :element-type '(signed-byte 16))))
               (with-foreign-objects ((nread 'size_t)
                                      (cbuffer :short 16384))
                 (loop as err = (mpg123-read handle cbuffer 32768 nread)
                       as num-samples = (ash (mem-ref nread 'size_t) -1)
                       with idx = 0
                       while (and (zerop err) (<= idx length)) do
                       (loop for outidx from idx below (min length (+ idx num-samples))
                             for index from 0 below num-samples do
                             (setf (aref buffer outidx)
                                   (mem-aref cbuffer :short index)))
                       (incf idx num-samples)
                       finally
                       (when (and (< idx length) (/= err -12))  ; MPG123_DONE?
                         (cerror "Ignore it." "Unexpected decoding error ~D!" err))
                       (return-from decode-mp3-file
                         (values buffer rate channels encoding))))))
        (mpg123-close handle)
        (mpg123-delete handle)))))
