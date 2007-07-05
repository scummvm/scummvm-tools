;;;; util.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-05 01:03:01 brx>

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

;;;; abbrev top level bindings

(define ash arithmetic-shift)

(define bxor bitwise-xor)
(define bior bitwise-ior)
(define band bitwise-and)
(define bnot bitwise-not)

;;;; fp procedures

(define (compose f g)
  (lambda (x) (f (g x))))

(define (hole f)
  (lambda x (f)))

;;;; num procedures

(define (between? x a b)
  (and (>= x a) (<= x b)))

(define /= (complement =))

;;;; list procedures

(define (generate-subsets lis)
  (let accum-subsets ((lis lis)
                      (accum '(())))
    (if (null? lis)
        accum
        (accum-subsets (cdr lis)
                       (append (map (cut cons (car lis) <>)
                                    accum)
                               accum)))))

;;;; port reader procedures

(define (read-u8 port)
  (let ((char (read-char port)))
    (if (eof-object? char)
        (abort 'eof)
        (char->integer char))))

(define (read-le-u16 port)
  (bior (read-u8 port)
        (ash (read-u8 port) 8)))

(define (read-be-u32 port)
  (bior (ash (read-u8 port) 24)
        (ash (read-u8 port) 16)
        (ash (read-u8 port) 8)
        (read-u8 port)))

;;;; data conversion procedures

(define (string->u32 string)
  (fold (lambda (h t)
          (+ (char->integer h)
             (ash t 8)))
        0
        (string->list string)))
