;;;; cfgg.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-31 21:19:30 brx>

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

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")

(define-record-type loop-info
  (make-loop-info head latch type follow)
  loop-info?
  (head loop-head set-loop-head!)
  (latch loop-latch set-loop-latch!)
  (type loop-type set-loop-type!)
  (follow loop-follow set-loop-follow!))

(define-record-type basic-block
  (make-basic-block type range post-order loop-info follow)
  basic-block?
  (type bb-type set-bb-type!)
  (range bb-range set-bb-range!)
  (post-order post-order set-post-order!)
  (loop-info loop-info set-loop-info!)
  (follow bb-follow set-bb-follow!))

(define basic-block
  (cut make-basic-block <> <> #f (make-loop-info #f #f #f #f) #f))

(define-record-printer (basic-block x out)
  (fprintf out "(basic-block ~A ~A ~A)" (post-order x) (bb-type x) (bb-range x)))

(define (bb-update! bb #!key type range)
  (when type (set-bb-type! bb type))
  (when range (set-bb-range! bb range))
  bb)

(define (get-trivial-block instructions)
  (let get-trivial ((addrs '())
                    (instrs instructions))
    (if (null? instrs)
        (values (basic-block 'return (reverse addrs))
                '())
        (match-let (((addr . op-info) (car instrs)))
          (case (car op-info)
            ((goto goto-unless)
             (values (basic-block op-info (reverse (cons addr addrs)))
                     (cdr instrs)))
            (else
             (get-trivial (cons addr addrs) (cdr instrs))))))))

(define (generate-trivial-blocks disassembly)
  (if (null? disassembly)
      '()
      (receive (block rest)
          (get-trivial-block disassembly)
        (cons block (generate-trivial-blocks rest)))))

(define (splice-block! block pivot)
  (receive (fall-range rest-range)
      (partition (cut < <> pivot) (bb-range block))
    (let ((fall-block (basic-block 'fall fall-range)))
      (values fall-block
              (bb-update! block range: rest-range)))))

(define (find/splice-block-at! jump-addr fblocks)
  (if (null? fblocks)
      '()
      (let* ((fblock (car fblocks))
             (fblock-range (bb-range fblock)))
        (cond ((eq? (car fblock-range) jump-addr) fblocks)
              ((memq jump-addr fblock-range)
               (call-with-values
                   (lambda ()
                     (splice-block! fblock jump-addr))
                 (cut cons* <> <> (cdr fblocks))))
              (else
               (cons fblock
                     (find/splice-block-at! jump-addr (cdr fblocks))))))))

(define (correct-blocks! blocks)
  (let loop ((blocks blocks)
             (fixed-blocks blocks))
    (if (null? blocks)
        fixed-blocks
        (match (bb-type (car blocks))
          (((or 'goto-unless 'goto) jump-addr . _)
           (loop (cdr blocks)
                 (find/splice-block-at! jump-addr fixed-blocks)))
          (else
           (loop (cdr blocks) fixed-blocks))))))

(define (blocks->cfg blocks)
  (define (target-block-index jump-addr)
    (list-index (o (cut memq jump-addr <>)
                   bb-range)
                blocks))
  (let ((g (make-digraph 'cfg "control flow graph"))
        (ii (list-tabulate (length blocks) identity)))
    (for-each (lambda (i block)
                ((g 'add-node!) i (list i block)))
              ii
              blocks)
    (for-each (lambda (i b)
                (let ((outs (match (bb-type b)
                              (('goto-unless jump-addr _)
                               (list (add1 i)
                                     (target-block-index jump-addr)))
                              (('goto jump-addr)
                               (list (target-block-index jump-addr)))
                              ('fall
                               (list (add1 i)))
                              (else #f))))
                  (when outs
                    (for-each (lambda (out)
                                (unless ((g 'has-edge) i out)
                                  ((g 'add-edge!)
                                   (list i out #f))))
                              outs))))
              ii
              blocks)
    g))

(define (inject-post-order! cfg)
  (let ((ninfo (cfg 'node-info))
        (po 0))
    (define (get-po!) (set! po (add1 po)) po)
    (for-each (lambda (npo)
                (let ((node (second (ninfo (first npo)))))
                  (set-post-order! node (get-po!))))
              (graph-postorder cfg 0))
    cfg))

(define (generate-control-flow-graph disassembly)
  (let ((cfg
         (remove-isolated!
          (blocks->cfg
           (correct-blocks!
            (generate-trivial-blocks
             (map (lambda (instruction)
                    (cons (car instruction) (cddr instruction)))
                  disassembly)))))))
    (values (inject-post-order! cfg)
            (generate-intervals cfg (list 0)))))

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")
