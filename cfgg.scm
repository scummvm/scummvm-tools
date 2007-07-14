;;;; cfgg.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-10 20:25:19 brx>

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

(define-record basic-block type range preds succs)

(define-record-printer (basic-block x out)
  (fprintf out
           "#,(basic-block ~S ~S ~S ~S)"
           (basic-block-type x)
           (basic-block-range x)
           (map basic-block-range (basic-block-preds x))
           (map basic-block-range (basic-block-succs x))))

(define (create-basic-block type range)
  (make-basic-block type range '() '()))

(define (basic-block-type-smart block)
  (let ((type (basic-block-type block)))
    (if (pair? type)
        (car type)
        type)))

(define (update-basic-block! block #!key type range preds succs)
  (when type (basic-block-type-set! block type))
  (when range (basic-block-range-set! block range))
  (when preds (basic-block-preds-set! block preds))
  (when succs (basic-block-succs-set! block succs))
  block)

(define (basic-block-connect! pred succ)
  (basic-block-succs-set! pred (cons succ (basic-block-succs pred)))
  (basic-block-preds-set! succ (cons pred (basic-block-preds succ)))
  succ)

(define (remove-opcodes-from-disassembly disassembly)
  (map (lambda (instruction)
         (cons (car instruction)
               (cddr instruction)))
       disassembly))

(define (get-trivial-block instructions)
  (let get-trivial ((addrs '())
                    (instrs instructions))
    (if (null? instrs)
        (values (create-basic-block 'return
                                    (reverse addrs))
                '())
        (let ((instr (car instrs)))
          (case (cadr instr)
            ((goto goto-unless)
             (values (create-basic-block (cdr instr)
                                         (reverse (cons (car instr)
                                                        addrs)))
                     (cdr instrs)))
            (else
             (get-trivial (cons (car instr) addrs)
                          (cdr instrs))))))))

(define (generate-trivial-blocks disassembly blocks connect?)
  (if (null? disassembly)
      (reverse blocks)
      (receive (trivial-block rest)
          (get-trivial-block disassembly)
        (when connect? (basic-block-connect! (car blocks) trivial-block))
        (generate-trivial-blocks
         rest
         (cons trivial-block blocks)
         (not
          (eq? 'goto
               (basic-block-type-smart trivial-block)))))))

(define (rewire-preds! block new-succ)
  (for-each (lambda (pred)
              (basic-block-succs-set!
               pred
               (cons new-succ
                     (delete block (basic-block-succs pred) eq?))))
            (basic-block-preds block)))

(define (splice-block! block pivot pred)
  (receive (fall-range rest-range)
      (partition (cut < <> pivot)
                 (basic-block-range block))
    (let ((fall-block (make-basic-block 'fall fall-range
                                        (basic-block-preds block)
                                        (list block))))
      (rewire-preds! block fall-block)
      (values
       fall-block
       (basic-block-connect! pred
                             (update-basic-block! block
                                                  range: rest-range
                                                  preds: (list fall-block)))))))

(define (find/splice-block! block jump-addr fblocks)
  (if (null? fblocks)
      '()
      (let* ((fblock (car fblocks))
             (fblock-range (basic-block-range fblock)))
        (cond ((eq? (car fblock-range) jump-addr)
               (cons (basic-block-connect! block fblock)
                     (cdr fblocks)))
              ((memq jump-addr fblock-range)
               (call-with-values
                   (lambda ()
                     (splice-block! fblock jump-addr block))
                 (cut cons* <> <> (cdr fblocks))))
              (else
               (cons fblock
                     (find/splice-block! block
                                         jump-addr
                                         (cdr fblocks))))))))

(define (correct-trivial-blocks! trivial-blocks fixed-blocks)
  (if (null? trivial-blocks)
      fixed-blocks
      (let* ((block (car trivial-blocks))
             (type (basic-block-type block))
             (smart-type (basic-block-type-smart block)))
        (if (or (eq? 'goto-unless smart-type)
                (eq? 'goto smart-type))
            (correct-trivial-blocks! (cdr trivial-blocks)
                                     (find/splice-block! block
                                                         (cadr type)
                                                         fixed-blocks))
            (correct-trivial-blocks! (cdr trivial-blocks)
                                     fixed-blocks)))))

(define (find-interval interval basic-blocks)
  (let ((new-interval-nodes (partition (lambda (block)
                                         (and (not (memq block interval))
                                              (every (cut memq <> interval)
                                                     (basic-block-preds block))))
                                       basic-blocks)))
    (if (null? new-interval-nodes)
        interval
        (find-interval (append interval
                               new-interval-nodes)
                       basic-blocks))))

(define (generate-intervals unprocessed-headers headers basic-blocks)
  (if (null? unprocessed-headers)
      '()
      (let* ((new-interval (find-interval (list (car unprocessed-headers))
                                          basic-blocks))
             (new-headers (partition (lambda (block)
                                       (and (not (memq block headers))
                                            (not (memq block new-interval))
                                            (any (cut memq <> new-interval)
                                                 (basic-block-preds block))))
                                     basic-blocks)))
        (cons new-interval
              (generate-intervals (append (cdr unprocessed-headers)
                                          new-headers)
                                  (append headers
                                          new-headers)
                                  basic-blocks)))))

(define (generate-control-flow-graph disassembly)
  (let* ((trivial-blocks
          (generate-trivial-blocks
           (remove-opcodes-from-disassembly disassembly) '() #f))
         (basic-blocks (correct-trivial-blocks! trivial-blocks
                                                trivial-blocks)))
    (values basic-blocks
            (generate-intervals (list (car basic-blocks))
                                (list (car basic-blocks))
                                basic-blocks))))

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")
