;;; antipasto.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-06-08 20:19:52 brx>

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; $URL: /local/gsoc2007-decompiler/antipasto.scm $
;;; $Id: /local/gsoc2007-decompiler/antipasto.scm 5 2007-05-27T18:38:05.723705Z brx  $

(require-extension posix)

(define ash arithmetic-shift)

(define (compose f g) (lambda (x) (f (g x))))

(define read-byte (compose char->integer read-char))

(define (read-u8 reader)
  (reader 'read-byte))

(define (read-be-u32 reader)
  (bitwise-ior (ash (read-u8 reader) 24)
               (ash (read-u8 reader) 16)
               (ash (read-u8 reader) 8)
               (read-u8 reader)))

(define (string->u32 string)
  (define (build-u32 char-list integer)
    (if (null? char-list)
        integer
        (build-u32 (cdr char-list)
                   (+ (ash integer 8)
                      (char->integer (car char-list))))))
  (build-u32 (string->list string) 0))

(define lscr (string->u32 "LSCR")) ; 9
(define scrp (string->u32 "SCRP")) ; 8
(define encd (string->u32 "ENCD")) ; 8
(define excd (string->u32 "EXCD")) ; 8
(define verb (string->u32 "VERB")) ; skipVerbHeader_V567

(define (make-script-reader file-name)
  (let ((port (open-input-file file-name))
        (size (file-size file-name)))
    (lambda (op)
      (case op
        ('read-byte (read-byte port))
        ('file-size size)
        ('file-name file-name)
        ('file-port port)
        ('eof (eof-object? ))
        ('close (close-input-port port))
        (else (error "script reader no-op" op))))))

(define (parse-local-script-header reader)
  (when (< (reader 'file-size) 9)
    (error (string-append (reader 'file-name) " is too small to be a local script")))
  (set-file-position! (reader 'file-port) 8)
  (print (string-append "Local Script #"
                        (number->string (read-u8 reader)))))

(define (parse-header reader)
  (when (< (reader 'file-size) 8)
    (error (string-append (reader 'file-name) " is too small to be a script")))
  (let ((script-type (read-be-u32 reader)))
    (cond
      ((= lscr script-type)
       (parse-local-script-header reader))
      ((= scrp script-type)
       'global-script)
      ((= encd script-type)
       'room-entry-script)
      ((= excd script-type)
       'room-exit-script)
      (else (error "unknown script type")))))

(define (decode-op reader op)
  op)

(define (decode-ops reader decoded)
  (let ((op (read-u8 reader)))
    (if (eof-object? op)
        (reverse decoded)
        (decodeOps reader (decode-op reader op)))))

(define test-script "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")

(define (test-run)
  (let ((reader (make-script-reader test-script)))
    (parse-header reader)
    (reader 'close)))
