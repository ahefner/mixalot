;;;; Common string manipulation and tag filtering functions

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

(defpackage :mixalot-strings-common
  (:use :cl :cffi)
  (:export #:safely-convert-string
           #:magic-string-conversion

           #:clean-tags))

(in-package :mixalot-strings-common)

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

(defun magic-string-conversion (c-string-ptr &key no-utf8)
  "Attempts to convert the pointer to a lisp string using UTF-8
  encoding. If that fails, converts it using ISO-8859-1. Returns two
  values: the converted lisp string, and the encoding used (one
  of :utf-8 or :iso-8859-1)."
  (let ((utf-8-string (and (not no-utf8)
                    (safely-convert-string c-string-ptr :utf-8))))
    (cond (utf-8-string (values utf-8-string :utf-8))
          (t (safely-convert-string c-string-ptr :iso-8859-1)))))


(defun clean-tags (properties)
  (when (> 1 (getf properties :track))
    (remf properties :track))
  ;; Remove tags for unknown artist or unknown disc, because that's a nuisance.
  (when (member (getf properties :artist)
                '("Unknown" "Unknown Artist" "<Unknown>")
                :test #'equalp)
    (remf properties :artist))
  (when (or (member (getf properties :album)
                    '("Unknown" "Unknown Disc" "<Unknown>")                      
                   :test #'equalp)
            (eql 14 (mismatch "Unknown Album " (getf properties :album))))
    (remf properties :album))
  ;; This is particularly stupid:
  (when (equalp "genre" (getf properties :genre))
    (remf properties :genre))
  (nconc properties))

(defun trim-if-string (value)
  (typecase value
    (string (string-trim " " value))
    (t value)))
