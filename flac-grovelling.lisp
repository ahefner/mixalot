;;;; CFFI grovelling file for libFLAC
;;;
;;; Sumant S.R. Oemrawsingh, 2010

(in-package :flac)

(include "FLAC/stream_decoder.h")
(cc-flags "-lFLAC" "-lm")

;;;; Basic types

(ctype flac-int16 "FLAC__int16")
(ctype flac-int32 "FLAC__int32")
(ctype flac-uint64 "FLAC__uint64")
(ctype flac-bool "FLAC__bool")
(ctype flac-unsigned "unsigned")

(ctype flac-decoder-init-status "FLAC__StreamDecoderInitStatus")
(ctype flac-decoder-error-status "FLAC__StreamDecoderErrorStatus")
(ctype flac-decoder-state "FLAC__StreamDecoderState")

(cenum flac-decoder-write-status
 ((:write-continue "FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE")
  :documentation "The write was OK and decoding can continue.")
 ((:write-abort "FLAC__STREAM_DECODER_WRITE_STATUS_ABORT")
  :documentation "An unrecoverable error occurred. The decoder will return from the process call."))

(cenum flac-metadata-type
  ((:stream-info "FLAC__METADATA_TYPE_STREAMINFO")
   :documentation "STREAMINFO block")
  ((:padding "FLAC__METADATA_TYPE_PADDING")
   :documentation "PADDING block")
  ((:application "FLAC__METADATA_TYPE_APPLICATION")
   :documentation "APPLICATION block")
  ((:seektable "FLAC__METADATA_TYPE_SEEKTABLE")
   :documentation "SEEKTABLE block")
  ((:vorbis-comment "FLAC__METADATA_TYPE_VORBIS_COMMENT")
   :documentation "VORBISCOMMENT block (a.k.a. FLAC tags)")
  ((:cuesheet "FLAC__METADATA_TYPE_CUESHEET")
   :documentation "CUESHEET block")
  ((:picture "FLAC__METADATA_TYPE_PICTURE")
   :documentation "PICTURE block")
  ((:undefined "FLAC__METADATA_TYPE_UNDEFINED")
   :documentation "marker to denote beginning of undefined type range; this number will increase as new metadata types are added"))

(cstruct flac-metadata-stream-info "FLAC__StreamMetadata_StreamInfo"
  (sample-rate "sample_rate" :type flac-unsigned)
  (channels "channels" :type flac-unsigned)
  (bits-per-sample "bits_per_sample" :type flac-unsigned)
  (total-samples "total_samples" :type flac-uint64))

;(cvar "FLAC__StreamDecoderErrorStatusString" :pointer :read-only t)

(cstruct flac-metadata-padding "FLAC__StreamMetadata_Padding")
(cstruct flac-metadata-application "FLAC__StreamMetadata_Application")
(cstruct flac-metadata-seektable "FLAC__StreamMetadata_SeekTable")
(cstruct flac-metadata-vorbiscomment "FLAC__StreamMetadata_VorbisComment")
(cstruct flac-metadata-cuesheet "FLAC__StreamMetadata_CueSheet")
(cstruct flac-metadata-picture "FLAC__StreamMetadata_Picture")
(cstruct flac-metadata-unknown "FLAC__StreamMetadata_Unknown")

(cstruct flac-frame-header "FLAC__FrameHeader"
  (block-size "blocksize" :type flac-unsigned)
  (channels "channels" :type flac-unsigned))

(cstruct flac-frame "FLAC__Frame"
  (frame-header "header" :type flac-frame-header))
