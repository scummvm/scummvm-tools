;;;; graph.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-31 21:04:36 brx>

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

;; ugly graphviz output stuff
(define (print-dot g disassembly intervals)
  (define (quote-string str)
    (string-translate* str '(("\"" . "\\\""))))
  (let ((nodes ((g 'nodes)))
        (edges ((g 'edges))))
    (print "digraph G { node [shape = box, fontsize = 10, fontname = Courier]")
    (for-each (lambda (n)
                (match-let (((n block) n))
                  (print* "    n"
                          n
                          " [label = \""
                          (quote-string (format "[~S] ~S"
                                                (car (bb-range block))
                                                (cddr (assq (car (bb-range block))
                                                            disassembly)))))
                  (for-each
                   (lambda (in)
                     (print* "\\l"
                             (quote-string (format "[~S] ~S"
                                                   in
                                                   (cddr (assq in disassembly))))))
                   (cdr (bb-range block)))
                  (print "\""
                         (cond ((zero? (car (bb-range block)))
                                ", shape=ellipse, style=bold]")
                               ((eq? 'return (bb-type block))
                                ", shape=ellipse, style=filled]")
                               (else "]")))))
              nodes)
    (when intervals
      (for-each
       (lambda (interval iter)
         (print "    subgraph cluster" (car interval) " {")
         (print "        label = \"I(" iter ")\"")
         (for-each (cut print "        n" <>) interval)
         (print "    }"))
       intervals
       (list-tabulate (length intervals) identity)))
    (for-each (lambda (e) (print "    n" (first e) " -> n" (second e))) edges)
    (print "}")))

(define (remove-isolated! g)
  (let loop ((repeat? #f))
    (for-each (lambda (root)
                ((g 'remove-node!) root)
                (set! repeat? #t))
              (delete 0 ((g 'roots)) eq?))
    (when repeat? (loop #f)))
  g)

(define (get-dominator-alist g)
  (let ((pred (g 'pred))
        (fen (g 'foreach-node))
        (dalist
         (cons (list 0 0)
               (map (lambda (n)
                      (cons (first n)
                            (list-copy (unzip1 ((g 'nodes))))))
                    (remove (o zero? first)
                            ((g 'nodes)))))))
    (let loop ((changed #f))
      (for-each (lambda (da)
                  (let* ((preds (pred (car da)))
                         (pre-doms (map (o cdr (cut assq <> dalist)) preds))
                         (new-doms
                          (lset-adjoin eq?
                                       (if (null? (cdr pre-doms))
                                           (car pre-doms)
                                           (apply lset-intersection eq? pre-doms))
                                       (car da))))
                    (unless (eq? (length new-doms)
                                 (length (cdr da)))
                      (set! changed #t)
                      (set-cdr! da new-doms))))
                (cdr dalist))
      (when changed
        (loop #f)))
    (map (lambda (da)
           (cons (car da)
                 (delete (car da) (cdr da) eq?)))
         (cdr dalist))))

(define (get-immed-dominator-alist g)
  (let ((tninfo (o second (g 'node-info))))
    (map (lambda (da)
           (cons (car da)
                 (fold (lambda (a z)
                         (if (> (post-order (tninfo a))
                                (post-order (tninfo z)))
                             a
                             z))
                       (cadr da)
                       (cddr da))))
         (get-dominator-alist g))))

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
  (let ((nodes (unzip1 ((g 'nodes))))
        (immed-preds (o unzip1 (g 'in-edges))))
    (let loop ((headers headers)
               (unproc-headers headers))
      (if (null? unproc-headers)
          '()
          (let* ((new-interval
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
                        (append (cdr unproc-headers) new-headers))))))))

(define (get-neighbour-intervals interval intervals neighbours selector)
  (delete-duplicates
   (map (lambda (n)
          ;; note that for outgoing neighbours should always be
          ;; interval headers (or else their containing subgraph
          ;; would not be single-entry, aka an interval)
          (let ((index (list-index (cut member n <>) intervals)))
            ;; assert that N is member of an interval ...
            (assert index) index))
        (lset-difference eq?
                         (delete-duplicates
                          (append-map (o (cut map selector <>) neighbours)
                                      interval)
                          eq?)
                         interval))
   eq?))

(define (derive-graph g ivs)
  (define get-neigh-ivs
    (cut get-neighbour-intervals <> ivs <> <>))
  (let* ((g+1 (make-digraph 'derived-graph (cons g ivs)))
         (ii (list-tabulate (length ivs) identity)))
    (for-each (cute (g+1 'add-node!) <> <>)
              ii
              (map (lambda (iv)
                     (cons (car ((g 'node-info) (car iv)))
                           (append-map (o cdr (g 'node-info)) iv)))
                   ivs))
    (for-each (lambda (i iv)
                (let ((sipreds (get-neigh-ivs iv (g 'in-edges) first))
                      (sisuccs (get-neigh-ivs iv (g 'out-edges) second)))
                  (for-each (lambda (j)
                              (unless ((g+1 'has-edge) j i)
                                ((g+1 'add-edge!) (list j i #f))))
                            sipreds)
                  (for-each (lambda (j)
                              (unless ((g+1 'has-edge) i j)
                                ((g+1 'add-edge!) (list i j #f))))
                            sisuccs)))
              ii
              ivs)
    (values g+1 (generate-intervals g+1 (list 0)))))

(define (generate-derived-graph-sequence g ivs)
  (cons (cons g ivs)
        (receive (g+1 ivs+1)
            (derive-graph g ivs)
          (if (eq? ((g+1 'order)) ((g 'order)))
              '()
              (generate-derived-graph-sequence g+1 ivs+1)))))
