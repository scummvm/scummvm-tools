;;;; structuring.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-31 21:21:37 brx>

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

(define (po->nn g node)
  (let ((po (post-order node))
        (ns ((g 'nodes))))
    (first (find (lambda (n)
                   (= po (post-order (second (second n)))))
                 ns))))

;;; Loop Structuring

(define (maybe-get-latching g header ivn)
  (let ((ninfo (g 'node-info)))
    (let find-latching ((ins (unzip1 ((g 'in-edges) header))))
      (cond ((null? ins) #f)
            ((memq (second (ninfo (car ins))) ivn) (second (ninfo (car ins))))
            (else (find-latching (cdr ins)))))))

(define (mark-nodes! latching header ivn)
  (let ((header-po (post-order header))
        (latching-po (post-order latching)))
    (let mark ((ivn ivn)
               (loop-nodes '()))
      (if (null? ivn)
          loop-nodes
          (let* ((node (car ivn))
                 (linfo (loop-info node)))
            (if (<= header-po (post-order node) latching-po)
                (begin
                  (when (not (loop-head linfo))
                    (set-loop-head! linfo header)
                    (set-loop-latch! linfo latching))
                  (mark (cdr ivn) (cons node loop-nodes)))
                (mark (cdr ivn) loop-nodes)))))))

(define (choose-loop-type! latching header hnum nodes-in-loop g)
  (let ((oedges (g 'out-edges))
        (ninfo (g 'node-info)))
    (set-loop-type! (loop-info header)
                    (match (cons (bb-type latching) (bb-type header))
                      ((('goto-unless . _) . ('goto-unless . _))
                       (let ((oe (map second (oedges hnum))))
                         (if (and (memq (second (ninfo (first oe))) nodes-in-loop)
                                  (memq (second (ninfo (second oe))) nodes-in-loop))
                             'post-tested
                             'pre-tested)))
                      ((('goto-unless . _) . (or ('goto . _) 'fall 'return))
                       'post-tested)
                      (((or ('goto . _ ) 'fall 'return) . ('goto-unless . _))
                       'pre-tested)
                      (((or ('goto . _) 'fall 'return) . (or ('goto . _) 'fall 'return))
                       'endless)))))

(define (choose-loop-follow! latching lnum header hnum nodes-in-loop g)
  (let ((oedges (g 'out-edges))
        (ninfo (g 'node-info)))
    (set-loop-follow! (loop-info header)
                      (case (loop-type (loop-info header))
                        ((pre-tested)
                         (let ((oe (map second (oedges hnum))))
                           (if (memq (second (ninfo (first oe))) nodes-in-loop)
                               (second (ninfo (second oe)))
                               (second (ninfo (first oe))))))
                        ((post-tested)
                         (let ((oe (map second (oedges lnum))))
                           (if (memq (second (ninfo (first oe))) nodes-in-loop)
                               (second (ninfo (second oe)))
                               (second (ninfo (first oe))))))
                        (else
                         (let ((fol (expt 2 32))
                               (cn #f))
                           (for-each (lambda (twn)
                                       (let ((oe (map second (oedges (po->nn g twn)))))
                                         (cond
                                           ((and (not (memq (second (ninfo (first oe)))
                                                            nodes-in-loop))
                                                 (< (post-order (second (ninfo (first oe)))) fol))
                                            (set! cn (second (ninfo (second oe))))
                                            (set! fol (post-order (second (ninfo (first oe))))))
                                           ((and (not (memq (second (ninfo (second oe)))
                                                            nodes-in-loop))
                                                 (< (post-order (second (ninfo (second oe)))) fol))
                                            (set! cn (second (ninfo (second oe))))
                                            (set! fol (post-order (second (ninfo (second oe)))))))))
                                     (filter (lambda (node)
                                               (match (bb-type node)
                                                 (('goto-unless . _) #t)
                                                 (else #f)))
                                             nodes-in-loop))
                           cn))))))

(define (structure-loops! top-graph dgs)
  (if (null? dgs)
      (void)
      (match-let ((((g . ivs) . rest) dgs))
        (for-each (lambda (iv)
                    (let* ((iv-nodes (append-map (o cdr (g 'node-info)) iv))
                           (header (car iv-nodes))
                           (hnum (car ((g 'node-info) (car iv))))
                           (latching (maybe-get-latching top-graph hnum iv-nodes)))
                      (when latching
                        (let ((nodes-in-loop (mark-nodes! latching header iv-nodes)))
                          (choose-loop-type! latching
                                             header
                                             hnum
                                             nodes-in-loop
                                             top-graph)
                          (choose-loop-follow! latching
                                               (po->nn top-graph latching)
                                               header
                                               hnum
                                               nodes-in-loop
                                               top-graph)))))
                  ivs)
        (structure-loops! top-graph rest))))

;;; Conditionals Structuring

(define (head-or-latch? node)
  (let ((linfo (loop-info node)))
    (or (eq? (loop-head linfo) node)
        (eq? (loop-latch linfo) node))))

(define (structure-2-way! top-graph)
  (let ((ninfo (o second (top-graph 'node-info)))
        (idoms (get-immed-dominator-alist top-graph)))
    (let loop ((nodes (unzip1
                       (sort (graph-postorder top-graph 0)
                             (lambda (a b)
                               (< (second a) (second b))))))
               (unresolved '()))
      (if (null? nodes)
          (void)
          (let ((node (car nodes)))
            (if (not (head-or-latch? (ninfo node)))
                (let ((ns (unzip1 (partition (lambda (idom)
                                               (eq? node (cdr idom)))
                                             idoms))))
                  (if (null? ns)
                      (loop (cdr nodes)
                            (cons node unresolved))
                      (let ((m (car
                                (sort ns
                                      (lambda (a b)
                                        (> (post-order (ninfo a))
                                           (post-order (ninfo b))))))))
                        (for-each (lambda (ur)
                                    (set-bb-follow! (ninfo ur)
                                                    (ninfo m)))
                                  unresolved)
                        (set-bb-follow! (ninfo node)
                                        (ninfo m))
                        (loop (cdr nodes) '()))))
                (loop (cdr nodes) unresolved)))))))

;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp")
;; (test-run "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")
