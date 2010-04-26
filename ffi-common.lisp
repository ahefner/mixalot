(defpackage :mixalot-ffi-common
  (:use :cl :cffi)
  (:export #:size_t
           #:off_t
           #:loff_t
           #:SEEK_SET
           #:SEEK_CUR
           #:SEEK_END
           #:memset
           #:valid-pointer
           #:validate-pointer))

(in-package :mixalot-ffi-common)

;;;; This is a bit of a fragile kludge, and anyone sane would use
;;;; CFFI-GROVEL or the like to pull these from the system headers. I
;;;; personally think that's an unacceptable burden for such basic
;;;; definitions, and will soldier along this way as long as I can.

#-CFFI-FEATURES:X86-64
(defctype size_t :unsigned-int)
#+CFFI-FEATURES:X86-64
(defctype size_t :uint64)

#-CFFI-FEATURES:X86-64
(defctype off_t :int)
#+CFFI-FEATURES:X86-64
(defctype off_t :int64)

(defctype loff_t :long-long)

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(defcfun memset :pointer
  (ptr :pointer)
  (constant :int)
  (size size_t))

(defun valid-pointer (ptr) (unless (null-pointer-p ptr) ptr))

(defun validate-pointer (ptr)
  (or (valid-pointer ptr)
      (error "Unexpected NULL pointer")))
