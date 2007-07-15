;;;; graph.scm

;;; Antipasto - Scumm Script Disassembler Prototype
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-15 05:39:43 brx>

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
  (print "digraph G { node [shape = box, fontsize = 10, fontname = Courier]")
  (let ((nodes ((g 'nodes))))
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
    (newline)
    (if intervals
        (for-each
         (lambda (interval iter)
           (let ((extern '()))
             (print "subgraph cluster" (car interval) " {")
             (print "    label = \"I(" iter ")\"")
             (for-each
              (lambda (i)
                (receive (intern ext)
                    (partition (cut member <> interval)
                               (map second ((g 'out-edges) i)))
                  (for-each (lambda (ij)
                              (print "    n" i " -> n" ij))
                            intern)
                  (set! extern (append extern (map (cut cons i <>) ext)))))
              interval)
             (print "}")
             (for-each (lambda (e)
                         (print "    n" (car e) " -> n" (cdr e)))
                       extern)))
         intervals
         (list-tabulate (length intervals) identity))
        (for-each (lambda (e)
                    (match-let (((i j _) e))
                      (print "    n" i " -> n" j)))
                  ((g 'edges)))))
  (print "}"))

(define (remove-isolated! g)
  (let loop ()
    (let ((repeat? #f))
      (for-each (lambda (root)
                  ((g 'remove-node!) root)
                  (set! repeat? #t))
                (delete 0 ((g 'roots)) eq?))
      (when repeat? (loop))))
  g)

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
    (for-each (cut (g+1 'add-node!) <> <>) ii ivs)
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
