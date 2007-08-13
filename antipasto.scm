;;;; antipasto.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-08-02 00:00:01 brx>

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

(require-extension srfi-1 posix numbers digraph graph-dfs fmt)

(include "util.scm")
(include "graph.scm")
(include "cfgg.scm")
(include "structuring.scm")
(include "pseudo.scm")

(define opcode-register (make-hash-table))

(define (register-opcode name code handler)
  (hash-table-set! opcode-register code (cons name handler)))

(define (register-complex-opcode name codes handler)
  (for-each (cut register-opcode name <> handler) codes))

(include "scummv5.scm")

(define current-script-file #f)
(define current-script-port #f)
(define current-script-offset #f)

(define (decode-op op)
  (let ((opcode-handler (hash-table-ref/default opcode-register
                                                op
                                                #f)))
    (if opcode-handler
        (cons* (sub1 current-script-offset)
               op
               (car opcode-handler)
               ((cdr opcode-handler) op))
        #f)))

(define (decode-ops decoded)
  (handle-exceptions exn
                     (and (eq? 'eof exn) (reverse decoded))
                     (let ((decoded-op (decode-op (fetch-byte))))
                       (if decoded-op
                           (decode-ops (cons decoded-op decoded))
                           (reverse decoded)))))

(define (test-run file)
  (set! current-script-file file)
  (set! current-script-port (open-input-file current-script-file))
  (set! current-script-offset 0)
  (parse-header)
  (let ((disassembly (decode-ops '())))
    (receive (cfg intervals)
        (generate-control-flow-graph disassembly)
;;       (print-dot cfg disassembly intervals)
;;       (newline)
      (structure-loops! cfg (generate-derived-graph-sequence cfg intervals))
      (structure-2-way! cfg)
      (pseudo-out (current-output-port) cfg disassembly)))
  (close-input-port current-script-port)
  (set! current-script-port #f)
  (set! current-script-file #f)
  (set! current-script-offset #f))

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")

(define (main)
  (if (= (length (argv)) 2)
      (test-run (cadr (argv)))
      (let ((script-file (car (argv))))
        (printf "Usage: ~A <scummV5 script>~%" script-file))))
