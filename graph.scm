;;;; graph.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-14 02:21:16 brx>

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

(define (find-interval nodes immed-preds interval)
  (let ((new-inodes
         (partition (lambda (n)
                      (if (memq n interval)
                          #f
                          (let ((ipreds (immed-preds n)))
                            (and (not (null? ipreds))
                                 (every (cut memq <> interval) ipreds)))))
                    nodes)))
    (if (null? new-inodes)
        interval
        (find-interval nodes immed-preds (append interval new-inodes)))))

(define (generate-intervals g headers)
  (let loop ((headers headers)
             (unproc-headers headers))
    (if (null? unproc-headers)
        '()
        (let* ((nodes (unzip1 ((g 'nodes))))
               (immed-preds (o unzip1 (g 'in-edges)))
               (new-interval
                (find-interval nodes immed-preds (list (car unproc-headers))))
               (new-headers
                (partition (lambda (n)
                             (and (not (memq n headers))
                                  (not (memq n new-interval))
                                  (any (cut memq <> new-interval)
                                       (immed-preds n))))
                           nodes)))
          (cons new-interval
                (loop (append headers new-headers)
                      (append (cdr unproc-headers) new-headers)))))))
