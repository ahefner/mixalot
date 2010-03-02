(defpackage :sndfile
  (:use :common-lisp :cffi :mixalot-ffi-common))

(in-package :sndfile)

(define-foreign-library libsndfile
  (:unix "libsndfile.so.1")
  (t (:default "libsndfile")))

(use-foreign-library libsndfile)

#+linux   (defctype sf_count_t loff_t)

;;; Untested:
#+bsd     (defctype sf_count_t off_t)
#+solaris (defctype sf_count_t :int64)
#+windows (defctype sf_count_t :int64)

(defcstruct (SF_INFO :conc-name sf-info-)
  (frames     loff_t)
  (samplerate :int)
  (channels   :int)
  (format     :int)
  (sections   :int)
  (seekable   :int))

(defconstant SF_FALSE    0)
(defconstant SF_TRUE     1)
(defconstant SFM_READ  #x10)
(defconstant SFM_WRITE #x20)
(defconstant SFM_RDRW  #x30)

(defctype SNDFILE* :pointer)
(defctype SF_INFO* (:pointer SF_INFO))

(defcfun sf-open SNDFILE*
  (path :string)
  (mode :int)
  (sfinfo SF_INFO*))

(defcfun sf-open-fd SNDFILE*
  (fd         :int)
  (mode       :int)
  (sfinfo     SF_INFO*)
  (close-fd-p :int))

;;; Returns an error number.
(defcfun sf-error :int (sndfile SNDFILE*))

;;; Returns current error as string.
(defcfun sf-strerror :string (sndfile SNDFILE*))

;;; Converts an error number to a string.

(defcfun sf-error-number :string (errnum :int))

;;;; Major formats.

(defvar *type-constants* nil)
(defvar *subtype-constants* nil)

(defmacro define-format-type (name value)
  `(progn
     (defconstant ,name ,value)
     (pushnew ',name *type-constants*)))

(defmacro define-format-subtype (name value)
  `(progn
     (defconstant ,name ,value)
     (pushnew ',name *subtype-constants*)))

(define-format-type SF_FORMAT_WAV                    #x010000)               ; Microsoft WAV format (little endian default). 
(define-format-type SF_FORMAT_AIFF                   #x020000)               ; Apple/SGI AIFF format (big endian). 
(define-format-type SF_FORMAT_AU                     #x030000)               ; Sun/NeXT AU format (big endian). 
(define-format-type SF_FORMAT_RAW                    #x040000)               ; RAW PCM data. 
(define-format-type SF_FORMAT_PAF                    #x050000)               ; Ensoniq PARIS file format. 
(define-format-type SF_FORMAT_SVX                    #x060000)               ; Amiga IFF / SVX8 / SV16 format. 
(define-format-type SF_FORMAT_NIST                   #x070000)               ; Sphere NIST format. 
(define-format-type SF_FORMAT_VOC                    #x080000)               ; VOC files. 
(define-format-type SF_FORMAT_IRCAM                  #x0A0000)               ; Berkeley/IRCAM/CARL 
(define-format-type SF_FORMAT_W64                    #x0B0000)               ; Sonic Foundry's 64 bit RIFF/WAV 
(define-format-type SF_FORMAT_MAT4                   #x0C0000)               ; Matlab (tm) V4.2 / GNU Octave 2.0 
(define-format-type SF_FORMAT_MAT5                   #x0D0000)               ; Matlab (tm) V5.0 / GNU Octave 2.1 
(define-format-type SF_FORMAT_PVF                    #x0E0000)               ; Portable Voice Format 
(define-format-type SF_FORMAT_XI                     #x0F0000)               ; Fasttracker 2 Extended Instrument 
(define-format-type SF_FORMAT_HTK                    #x100000)               ; HMM Tool Kit format 
(define-format-type SF_FORMAT_SDS                    #x110000)               ; Midi Sample Dump Standard 
(define-format-type SF_FORMAT_AVR                    #x120000)               ; Audio Visual Research 
(define-format-type SF_FORMAT_WAVEX                  #x130000)               ; MS WAVE with WAVEFORMATEX 
(define-format-type SF_FORMAT_SD2                    #x160000)               ; Sound Designer 2 
(define-format-type SF_FORMAT_FLAC                   #x170000)               ; FLAC lossless file format 
(define-format-type SF_FORMAT_CAF                    #x180000)               ; Core Audio File format 

;;;; Subtypes from here on. 

(define-format-subtype SF_FORMAT_PCM_S8                 #x0001)                 ; Signed 8 bit data 
(define-format-subtype SF_FORMAT_PCM_16                 #x0002)                 ; Signed 16 bit data 
(define-format-subtype SF_FORMAT_PCM_24                 #x0003)                 ; Signed 24 bit data 
(define-format-subtype SF_FORMAT_PCM_32                 #x0004)                 ; Signed 32 bit data 
(define-format-subtype SF_FORMAT_PCM_U8                 #x0005)                 ; Unsigned 8 bit data (WAV and RAW only) 

(define-format-subtype SF_FORMAT_FLOAT                  #x0006)                 ; 32 bit float data 
(define-format-subtype SF_FORMAT_DOUBLE                 #x0007)                 ; 64 bit float data 

(define-format-subtype SF_FORMAT_ULAW                   #x0010)                 ; U-Law encoded. 
(define-format-subtype SF_FORMAT_ALAW                   #x0011)                 ; A-Law encoded. 
(define-format-subtype SF_FORMAT_IMA_ADPCM              #x0012)                 ; IMA ADPCM. 
(define-format-subtype SF_FORMAT_MS_ADPCM               #x0013)                 ; Microsoft ADPCM. 

(define-format-subtype SF_FORMAT_GSM610                 #x0020)                 ; GSM 6.10 encoding. 
(define-format-subtype SF_FORMAT_VOX_ADPCM              #x0021)                 ; OKI / Dialogix ADPCM 

(define-format-subtype SF_FORMAT_G721_32                #x0030)                 ; 32kbs G721 ADPCM encoding. 
(define-format-subtype SF_FORMAT_G723_24                #x0031)                 ; 24kbs G723 ADPCM encoding. 
(define-format-subtype SF_FORMAT_G723_40                #x0032)                 ; 40kbs G723 ADPCM encoding. 

(define-format-subtype SF_FORMAT_DWVW_12                #x0040)                 ; 12 bit Delta Width Variable Word encoding. 
(define-format-subtype SF_FORMAT_DWVW_16                #x0041)                 ; 16 bit Delta Width Variable Word encoding. 
(define-format-subtype SF_FORMAT_DWVW_24                #x0042)                 ; 24 bit Delta Width Variable Word encoding. 
(define-format-subtype SF_FORMAT_DWVW_N                 #x0043)                 ; N bit Delta Width Variable Word encoding. 

(define-format-subtype SF_FORMAT_DPCM_8                 #x0050)                 ; 8 bit differential PCM (XI only)
(define-format-subtype SF_FORMAT_DPCM_16                #x0051)                 ; 16 bit differential PCM (XI only)

(defun decode-bitflags (value flag-names)
  (loop for symbol in flag-names
        as flag-value = (symbol-value symbol)
        when (= flag-value (logand value flag-value))
        collect symbol))

(defun match-field (value flag-names)
 (loop for symbol in flag-names
       when (= value (symbol-value symbol))
       return symbol))
 
;;;; Endian-ness options. 

(defconstant SF_ENDIAN_FILE                   #x00000000)     ; Default file endian-ness. 
(defconstant SF_ENDIAN_LITTLE                 #x10000000)     ; Force little endian-ness. 
(defconstant SF_ENDIAN_BIG                    #x20000000)     ; Force big endian-ness. 
(defconstant SF_ENDIAN_CPU                    #x30000000)     ; Force CPU endian-ness. 

(defconstant SF_FORMAT_SUBMASK                #x0000FFFF)
(defconstant SF_FORMAT_TYPEMASK               #x0FFF0000)
(defconstant SF_FORMAT_ENDMASK                #x30000000)

;;; Returns nonzero if the fields of SF_INFO structure are valid.
(defcfun sf-format-check :int (info SF_INFO*))

;;; Seek within waveform data. 'whence' is as with fseek.
(defcfun sf-seek sf_count_t
  (sndfile SNDFILE*)
  (num-frames sf_count_t)
  (whence :int))

(defcfun sf-set-string :int
  (sndfile SNDFILE*)
  (str_type :int)
  (string :string))

(defcfun sf-get-string :string
  (sndfile SNDFILE*)
  (str_type :int))

;;; Raw reading/writing, without automatic format conversion.

(defcfun sf-read-raw sf_count_t
  (sndfile SNDFILE*)
  (ptr     :pointer)
  (bytes   sf_count_t))

(defcfun sf-write-raw sf_count_t
  (sndfile SNDFILE*)
  (ptr     :pointer)
  (bytes   sf_count_t))

;;; Functions for reading and writing frames of data.
;;; A frame contains one item (sample) from each channel. *** This may be wrong. ***

(defcfun sf-readf-short sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :short))
  (frames  sf_count_t))

(defcfun sf-writef-short sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :short))
  (frames  sf_count_t))

(defcfun sf-readf-int sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :int))
  (frames  sf_count_t))

(defcfun sf-writef-int sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :int))
  (frames  sf_count_t))

(defcfun sf-readf-float sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :float))
  (frames  sf_count_t))

(defcfun sf-writef-float sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :float))
  (frames  sf_count_t))

(defcfun sf-readf-double sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :double))
  (frames  sf_count_t))

(defcfun sf-writef-double sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :double))
  (frames  sf_count_t))

;;; Functions for reading and writing items of data.
;;; Item counts must be an integer mulitple of the number of channels.

(defcfun sf-read-short sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :short))
  (items   sf_count_t))

(defcfun sf-write-short sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :short))
  (items   sf_count_t))

(defcfun sf-read-int sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :int))
  (items   sf_count_t))

(defcfun sf-write-int sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :int))
  (items   sf_count_t))

(defcfun sf-read-float sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :float))
  (items   sf_count_t))

(defcfun sf-write-float sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :float))
  (items   sf_count_t))

(defcfun sf-read-double sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :double))
  (items   sf_count_t))

(defcfun sf-write-double sf_count_t
  (sndfile SNDFILE*)
  (ptr     (:pointer :double))
  (items   sf_count_t))

;;; Close the SNDFILE and release allocated memory.
(defcfun sf-close :int (sndfile SNDFILE*))

;;; Sync written data to disk.
(defcfun sf-write-sync :void (sndfile SNDFILE*))

;;;; String types. Strings are entirely optional.

(defconstant SF_STR_TITLE     1)
(defconstant SF_STR_COPYRIGHT 2)
(defconstant SF_STR_SOFTWARE  3)
(defconstant SF_STR_ARTIST    4)
(defconstant SF_STR_COMMENT   5)
(defconstant SF_STR_DATE      6)

;;;; Public error numbers
(defconstant SF_ERR_NO_ERROR               0)
(defconstant SF_ERR_UNRECOGNIZED_FORMAT    1)
(defconstant SF_ERR_SYSTEM                 2)
(defconstant SF_ERR_MALFORMED_FILE         3)
(defconstant SF_ERR_UNSUPPORTED_ENCODING   4)

;;;; Playground area

(defcenum sfc-commands
  (:SFC_GET_LIB_VERSION                  #x1000)
  (:SFC_GET_LOG_INFO                     #x1001)
  (:SFC_GET_NORM_DOUBLE                  #x1010)
  (:SFC_GET_NORM_FLOAT                   #x1011)
  (:SFC_SET_NORM_DOUBLE                  #x1012)
  (:SFC_SET_NORM_FLOAT                   #x1013)
  (:SFC_SET_SCALE_FLOAT_INT_READ         #x1014)
  (:SFC_GET_SIMPLE_FORMAT_COUNT          #x1020)
  (:SFC_GET_SIMPLE_FORMAT                #x1021)
  (:SFC_GET_FORMAT_INFO                  #x1028)
  (:SFC_GET_FORMAT_MAJOR_COUNT           #x1030)
  (:SFC_GET_FORMAT_MAJOR                 #x1031)
  (:SFC_GET_FORMAT_SUBTYPE_COUNT         #x1032)
  (:SFC_GET_FORMAT_SUBTYPE               #x1033)
  (:SFC_CALC_SIGNAL_MAX                  #x1040)
  (:SFC_CALC_NORM_SIGNAL_MAX             #x1041)
  (:SFC_CALC_MAX_ALL_CHANNELS            #x1042)
  (:SFC_CALC_NORM_MAX_ALL_CHANNELS       #x1043)
  (:SFC_GET_SIGNAL_MAX                   #x1044)
  (:SFC_GET_MAX_ALL_CHANNELS             #x1045)
  (:SFC_SET_ADD_PEAK_CHUNK               #x1050)
  (:SFC_UPDATE_HEADER_NOW                #x1060)
  (:SFC_SET_UPDATE_HEADER_AUTO           #x1061)
  (:SFC_FILE_TRUNCATE                    #x1080)
  (:SFC_SET_RAW_START_OFFSET             #x1090)
  (:SFC_SET_DITHER_ON_WRITE              #x10A0)
  (:SFC_SET_DITHER_ON_READ               #x10A1)
  (:SFC_GET_DITHER_INFO_COUNT            #x10A2)
  (:SFC_GET_DITHER_INFO                  #x10A3)
  (:SFC_GET_EMBED_FILE_INFO              #x10B0)
  (:SFC_SET_CLIPPING                     #x10C0)
  (:SFC_GET_CLIPPING                     #x10C1)
  (:SFC_GET_INSTRUMENT                   #x10D0)
  (:SFC_SET_INSTRUMENT                   #x10D1)
  (:SFC_GET_LOOP_INFO                    #x10E0)
  (:SFC_GET_BROADCAST_INFO               #x10F0)
  (:SFC_SET_BROADCAST_INFO               #x10F1))

(defcfun sf-command :int
  (sndfile SNDFILE*)
  (command sfc-commands)
  (data :pointer)
  (datasize :int))

(defcenum SF_LOOP
  (:none 0)                             ; kludge
  (:SF_LOOP_NONE 800)
   :SF_LOOP_FORWARD
   :SF_LOOP_BACKWARD
   :SF_LOOP_ALTERNATING)

(defcstruct (SF_LOOP_INFO :conc-name sf-loop-info-)
  (time-sig-numerator :short)
  (time-sig-denominator :short)
  (loop-mode SF_LOOP)
  (num-beats :int)
  (bpm :float)                          ; Quarter notes per minute
  (root-key :int)
  (future :int :count 6))

(defcstruct (SF_INSTRUMENT_LOOP :conc-name sf-instrument-loop-)
  (mode  SF_LOOP)
  (start :unsigned-int)
  (end   :unsigned-int)
  (count :unsigned-int))

(defcstruct (SF_INSTRUMENT :conc-name sf-instrument-)
  (gain :int)
  (basenote :char)
  (detune :char)
  (velocity-low :char)
  (velocity-high :char)
  (key-low :char)
  (key-high :char)
  (loopcount :int)
  (loops SF_INSTRUMENT_LOOP :count 16))

(defun sfc-get-lib-version ()
  (with-foreign-object (buffer :char 512)
    (sf-command (null-pointer) :SFC_GET_LIB_VERSION buffer 512)
    (foreign-string-to-lisp buffer)))

(defun sfc-get-log-info (sndfile)
  (with-foreign-object (buffer :char 16384)
    (sf-command sndfile :SFC_GET_LOG_INFO buffer 16384)
    (foreign-string-to-lisp buffer)))

(defun sfc-update-header-now (sndfile)
  (sf-command sndfile :SFC_UPDATE_HEADER_NOW (null-pointer) 0))

(defun sfc-set-norm-float (sndfile enabled-p)
  (sf-command sndfile :SFC_SET_NORM_FLOAT (null-pointer) (if enabled-p SF_TRUE SF_FALSE)))

(defun sfc-set-norm-double (sndfile enabled-p)
  (sf-command sndfile :SFC_SET_NORM_DOUBLE (null-pointer) (if enabled-p SF_TRUE SF_FALSE)))

(defun sfc-get-norm-float (sndfile)
  (not (zerop (sf-command sndfile :SFC_GET_NORM_FLOAT (null-pointer) 0))))

(defun sfc-get-norm-double (sndfile)
  (not (zerop (sf-command sndfile :SFC_GET_NORM_DOUBLE (null-pointer) 0))))

(defun sfc-get-signal-max (sndfile)
  (with-foreign-object (max :double)
    (and (not (zerop (sf-command sndfile
                                 :SFC_GET_SIGNAL_MAX 
                                 max (foreign-type-size :double))))
         (convert-from-foreign max :double))))

