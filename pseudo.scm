;;;; pseudo.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-08-02 00:05:50 brx>

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

(define (output out ind str . args)
  (apply fprintf out (string-append (make-string (* ind 4)) str) args))

(define (write-dinfo out soff opc)
  (fprintf out
           "[~A] (~A) "
           (string-pad (->hex-string soff) 4 #\0)
           (string-pad (->hex-string opc) 2 #\0)))

(define (write-basic-block out ind block disassembly)
  (for-each (lambda (instr)
              (case (third instr)
                ((goto-unless goto) 'do-nothing)
                (else
                 (write-dinfo out (first instr) (second instr))
                 (output out ind (format "~S~%" (cddr instr))))))
            (map (cut assq <> disassembly)
                 (bb-range block))))

(define (is-loop-head? block)
  (eq? block (loop-head (loop-info block))))

(define (block->node block cfg)
  (first (find (lambda (n)
                 (eq? block (second (second n))))
               ((cfg 'nodes)))))

(define (write-loop out ind cfg eblock latch if-follow disasm)
  (let ((block-node (first eblock))
        (block (second eblock))
        (out-e (cfg 'out-edges))
        (succ (cfg 'succ))
        (ninfo (o second (cfg 'node-info))))
    (set-traversed! block #t)
    (let ((linfo (loop-info block)))
      ;; loop header
      (case (loop-type linfo)
        ((pre-tested)
         (write-basic-block out ind block disasm)
         (output out
                 ind
                 "(while ~A~%"
                 (if (eq? (ninfo (second (second (out-e block-node))))
                          (loop-follow linfo))
                     (third (bb-type block))
                     (list 'not (third (bb-type block))))))
        ((post-tested)
         (output out
                 ind
                 "(do-while ~A~%"
                 (third (bb-type block)))
         (write-basic-block out (add1 ind) block disasm))
        ((endless)
         (output out
                 ind
                 "(while #t~%")
         (write-basic-block out (add1 ind) block disasm)))
      ;; loop body
      (unless (or (eq? 'return (bb-type block))
                  (eq? block (loop-latch linfo)))
        (for-each (lambda (n)
                    (let ((s (ninfo n)))
                      (if (or (not (eq? 'pre-tested
                                        (loop-type linfo)))
                              (not (eq? s
                                        (loop-follow linfo))))
                          (if (traversed? s)
                              (output out 0 "(goto SOMEPLACE)~%")
                              (write-code out
                                          (add1 ind)
                                          cfg
                                          ((cfg 'node-info) n)
                                          (loop-latch linfo)
                                          if-follow
                                          disasm)))))
                  (succ block-node))
        (output out 0 ")~%")
        (if (or (not (loop-follow linfo))
                (traversed? (loop-follow linfo)))
            (output out 0 "(goto SOMEPLACE)~%")
            (write-code out
                        ind
                        cfg
                        (list (block->node (loop-follow linfo) cfg)
                              (loop-follow linfo))
                        latch
                        if-follow
                        disasm))))))

(define (write-2-way out ind cfg eblock latch if-follow disasm)
  (let* ((block-node (first eblock))
         (block (second eblock))
         (fl (bb-follow block))
         (out-edges (cfg 'out-edges))
         (ninfo (cfg 'node-info))
         (then (second (first (out-edges block-node))))
         (els (second (second (out-edges block-node))))
         (empty-then #f))
    (write-basic-block out ind block disasm)
    (if fl
        (begin
          (if (not (traversed? (second (ninfo then))))
              (if (not (eq? fl (second (ninfo then))))
                  (begin
                    (output out
                            (add1 ind)
                            "(if ~S~%(begin~%"
                            (third (bb-type block)))
                    (write-code out
                                (add1 ind)
                                cfg
                                (ninfo then)
                                latch
                                fl
                                disasm)
                    (output out (add1 ind) ")~%"))
                  (begin
                    (output out
                            (add1 ind)
                            "if ~S~%(begin~%"
                            (list 'not (third (bb-type block))))
                    (write-code out
                                (add1 ind)
                                cfg
                                (ninfo els)
                                latch
                                fl
                                disasm)
                    (set! empty-then #t)))
              (output out ind "(goto SOMEPLACE)~%"))
          (if (not (traversed? (second (ninfo els))))
              (when (not (eq? fl
                              (second (ninfo els))))
                (output out
                        (add1 ind)
                        "(begin~%")
                (write-code out
                            (add1 ind)
                            cfg
                            (ninfo els)
                            latch
                            fl
                            disasm))
              (unless empty-then
                (output out
                        (add1 ind)
                        "(begin (goto SOMEPLACE))~%")))
          (output out ind ")~%")
          (unless (traversed? fl)
            (write-code out
                        ind
                        cfg
                        (list (block->node fl cfg) fl)
                        latch
                        if-follow
                        disasm)))
        (output out ind "(SOME IF SHIT)~%"))))

(define (write-code out ind cfg eblock latch if-follow disasm)
  (let ((block-node (first eblock))
        (block (second eblock)))
    (unless (or (eq? (post-order block)
                     (and if-follow
                          (post-order if-follow)))
                (traversed? block))
      (set-traversed! block #t)
      (cond
        ((is-loop-head? block)
         (write-loop out ind cfg eblock latch if-follow disasm))
        ((and (list? (bb-type block))
              (eq? 'goto-unless
                   (car (bb-type block))))
         (write-2-way out ind cfg eblock latch if-follow disasm))
        (else
         (write-basic-block out ind block disasm)
         (unless (null? ((cfg 'succ) block-node))
           (let* ((s (first ((cfg 'succ) block-node)))
                  (seblock ((cfg 'node-info) s)))
             (if (traversed? (second seblock))
                 (output out ind "(goto SOMEPLACE)~%")
                 (write-code out ind cfg seblock latch if-follow disasm)))))))))

(define (pseudo-out out cfg disasm)
  (write-code out 0 cfg ((cfg 'node-info) 0) #f #f disasm))

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")
