;;;; CFFI grovelling file for libFLAC
;;;
;;; Sumant S.R. Oemrawsingh, 2010

(in-package :flac)

(include "FLAC/stream_decoder.h")
(cc-flags "-lFLAC" "-lm"
          #+darwin "-I/opt/local/include"
          #+darwin "-L/opt/local/lib")

;;;; Basic types

(ctype flac-int16 "FLAC__int16")
(ctype flac-int32 "FLAC__int32")
(ctype flac-uint32 "FLAC__uint32")
(ctype flac-uint64 "FLAC__uint64")
(ctype flac-bool "FLAC__bool")
(ctype flac-unsigned "unsigned")

(ctype flac-stream-decoder-init-status "FLAC__StreamDecoderInitStatus")

(cenum flac-stream-decoder-state
  ((:search-for-metadata "FLAC__STREAM_DECODER_SEARCH_FOR_METADATA")
   :documentation "The decoder is ready to search for metadata.")
  ((:read-metadata "FLAC__STREAM_DECODER_READ_METADATA")
   :documentation "The decoder is ready to or is in the process of reading metadata.")
  ((:search-for-frame-sync "FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC")
   :documentation "The decoder is ready to or is in the process of searching for the frame sync code.")
  ((:read-frame "FLAC__STREAM_DECODER_READ_FRAME")
   :documentation "The decoder is ready to or is in the process of reading a frame.")
  ((:end-of-stream "FLAC__STREAM_DECODER_END_OF_STREAM")
   :documentation "The decoder has reached the end of the stream.")
  ((:ogg-error "FLAC__STREAM_DECODER_OGG_ERROR")
   :documentation "An error occurred in the underlying Ogg layer.")
  ((:seek-error "FLAC__STREAM_DECODER_SEEK_ERROR")
   :documentation "An error occurred while seeking. The decoder must be flushed with FLAC__stream_decoder_flush() or reset with FLAC__stream_decoder_reset() before decoding can continue.")
  ((:aborted "FLAC__STREAM_DECODER_ABORTED")
   :documentation "The decoder was aborted by the read callback.")
  ((:memory-allocation-error "FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR")
   :documentation "An error occurred allocating memory. The decoder is in an invalid state and can no longer be used.")
  ((:uninitialized "FLAC__STREAM_DECODER_UNINITIALIZED")
   :documentation "The decoder is in the uninitialized state; one of the FLAC__stream_decoder_init_*() functions must be called before samples can be processed."))

(cenum flac-stream-decoder-write-status
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

(cenum flac-stream-decoder-error-status
  ((:lost-sync "FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC")
   :documentation "An error in the stream caused the decoder to lose synchronization.")
  ((:bad-header "FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER")
   :documentation "The decoder encountered a corrupted frame header.")
  ((:frame-crc-mismatch "FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH")
   :documentation "The frame's data did not match the CRC in the footer.")
  ((:unparseable-stream "FLAC__STREAM_DECODER_ERROR_STATUS_UNPARSEABLE_STREAM")
   :documentation "The decoder encountered reserved fields in use in the stream."))

(cstruct flac-metadata-stream-info "FLAC__StreamMetadata_StreamInfo"
  (minimum-block-size "min_blocksize" :type flac-unsigned)
  (maximum-block-size "max_blocksize" :type flac-unsigned)
  (sample-rate "sample_rate" :type flac-unsigned)
  (channels "channels" :type flac-unsigned)
  (bits-per-sample "bits_per_sample" :type flac-unsigned)
  (total-samples "total_samples" :type flac-uint64))


(cstruct flac-metadata-vorbis-comment-entry "FLAC__StreamMetadata_VorbisComment_Entry"
  (length "length" :type flac-uint32)
  (entry "entry" :type :pointer))

;(cvar "FLAC__StreamDecoderErrorStatusString" :pointer :read-only t)

;(cstruct flac-metadata-padding "FLAC__StreamMetadata_Padding")
;(cstruct flac-metadata-application "FLAC__StreamMetadata_Application")
;(cstruct flac-metadata-seektable "FLAC__StreamMetadata_SeekTable")
(cstruct flac-metadata-vorbis-comment "FLAC__StreamMetadata_VorbisComment"
  (vendor-string "vendor_string" :type flac-metadata-vorbis-comment-entry)
  (num-comments "num_comments" :type flac-uint32)
  (comments "comments" :type :pointer))
;(cstruct flac-metadata-cuesheet "FLAC__StreamMetadata_CueSheet")
;(cstruct flac-metadata-picture "FLAC__StreamMetadata_Picture")
;(cstruct flac-metadata-unknown "FLAC__StreamMetadata_Unknown")

(cstruct flac-frame-header "FLAC__FrameHeader"
  (block-size "blocksize" :type flac-unsigned)
  (channels "channels" :type flac-unsigned))

(cstruct flac-frame "FLAC__Frame"
  (frame-header "header" :type flac-frame-header))
