;;;; antipasto.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-06-25 05:05:15 brx>

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

(require-extension posix numbers srfi-1)

(include "util.scm")

(define current-script-file #f)
(define current-script-port #f)

(define opcode-register (make-hash-table))

(define (register-opcode name code handler)
  (hash-table-set! opcode-register code (cons name handler)))

(define (decode-op op)
  (let ((opcode-handler (hash-table-ref/default opcode-register
                                                op
                                                #f)))
    (if opcode-handler
        (cons (car opcode-handler) ((cdr opcode-handler) op))
        '(unknown shit))))

(define param-1 #x80)
(define param-2 #x40)
(define param-3 #x20)

(define (make-opcodes base-code flags)
  (if (null? flags)
      (list base-code)
      (cons (bior base-code (car flags))
            (make-opcodes base-code (cdr flags)))))

(define (make-123-op name base-code handler n)
  (when (between? n 0 3)
    (for-each (cut register-opcode name <> handler)
              (make-opcodes base-code
                            (take (list param-1 param-2 param-3) n)))))

(define (fetch-byte) (read-u8 current-script-port))
(define (fetch-word) (read-le-u16 current-script-port))

(define (get-var) (cons 'var (fetch-word)))

(define (get-var/byte op mask)
  (if (zero? (band op mask))
      (fetch-byte)
      (get-var)))

(define (get-var/word op mask)
  (if (zero? (band op mask))
      (fetch-word)
      (get-var)))

(define (get-arg-list)
  (let read-arg-list ((arg-list '())
                      (byte (fetch-byte)))
    (cond
      ((= byte #xff)
       (reverse arg-list))
      (else
       (read-arg-list (cons (get-var/word byte param-1)
                            arg-list)
                      (fetch-byte))))))

(define (get-ascii)
  (let read-ascii ((byte (fetch-byte))
                   (byte-list '()))
    (cond ((zero? byte)
           (list->string (map integer->char (reverse byte-list))))
          ((= byte #xff)
           (let ((a (fetch-byte)))
             (if (and (/= a 1)
                      (/= a 2)
                      (/= a 3)
                      (/= a 8))
                 (let ((b (fetch-byte))
                       (c (fetch-byte)))
                   (read-ascii (fetch-byte)
                               (cons c
                                     (cons b
                                           (cons a
                                                 (cons byte byte-list))))))
                 (read-ascii (fetch-byte)
                             (cons a (cons byte byte-list))))))
          (else
           (read-ascii (fetch-byte)
                       (cons byte byte-list))))))

(define (decode-parse-string)
  (let read-string ((string-infos '())
                    (byte (fetch-byte)))
    (cond
      ((= byte #xff)
       (reverse string-infos))
      ((= 0 (band byte #xf))
       (read-string (cons (list "Pos"
                                (get-var/word byte param-1)
                                (get-var/word byte param-2))
                          string-infos)
                    (fetch-byte)))
      ((= 1 (band byte #xf))
       (read-string (cons (list "Color" (get-var/byte byte param-1))
                          string-infos)
                    (fetch-byte)))
      ((= 2 (band byte #xf))
       (read-string (cons (list "Clipped" (get-var/word byte param-1))
                          string-infos)
                    (fetch-byte)))
      ((= 3 (band byte #xf))
       (read-string (cons (list "RestoreBG"
                                (get-var/word byte param-1)
                                (get-var/word byte param-2))
                          string-infos)
                    (fetch-byte)))
      ((= 4 (band byte #xf))
       (read-string (cons "Center" string-infos) (fetch-byte)))
      ((= 6 (band byte #xf))
       (read-string (cons "Left" string-infos) (fetch-byte)))
      ((= 7 (band byte #xf))
       (read-string (cons "Overhead" string-infos) (fetch-byte)))
      ((= 8 (band byte #xf))
       (read-string (cons (list "PlayCDTrack"
                                             (get-var/word byte param-1)
                                             (get-var/word byte param-2))
                                       string-infos)
                    (fetch-byte)))
      ((= 15 (band byte #xf))
       (read-string (cons (list "Text" (get-ascii)) string-infos)
                    #xff))
      (else
       (error "printEgo fucked up" string-infos)))))

(make-123-op "actorFollowCamera"
             #x52
             (compose list (cut get-var/byte <> param-1))
             1)

(register-opcode "cutscene" #x40 (compose list (hole get-arg-list)))

(make-123-op "animateActor"
             #x11
             (lambda (op)
               (list (get-var/byte param-1 op)
                     (get-var/byte param-2 op)))
             2)

(register-opcode "breakHere" #x80 (constantly '()))

(register-opcode "printEgo" #xd8 (compose list (hole decode-parse-string)))

(register-opcode "wait"
                 #xae
                 (lambda (_)
                   (let ((byte (fetch-byte)))
                     (cond ((or (= byte 1)
                                (= byte 81))
                            (list "for actor" (get-var/byte byte param-1)))
                           ((= byte 2)
                            (list "for message"))
                           ((= byte 3)
                            (list "for camera"))
                           ((= byte 4)
                            (list "for sentence"))
                           (else
                            (list "for something unknown"))))))

(register-opcode "delay"
                 #x2e
                 (lambda (_)
                   (list (bior (fetch-byte)
                               (ash (fetch-byte) 8)
                               (ash (fetch-byte) 16)))))

(define lscr (string->u32 "LSCR")) ; 9
(define scrp (string->u32 "SCRP")) ; 8
(define encd (string->u32 "ENCD")) ; 8
(define excd (string->u32 "EXCD")) ; 8
(define verb (string->u32 "VERB")) ; skipVerbHeader_V567

(define (parse-local-script-header)
  (when (< (file-size current-script-file) 9)
    (error (string-append current-script-file
                          " is too small to be a local script")))
  (set-file-position! current-script-port 8)
  (print (string-append "Local Script #"
                        (number->string (read-u8 current-script-port)))))

(define (parse-header)
  (when (< (file-size current-script-file) 8)
    (error (string-append current-script-file
                          " is too small to be a script")))
  (let ((script-type (read-be-u32 current-script-port)))
    (cond
      ((= lscr script-type)
       (parse-local-script-header))
      ((= scrp script-type)
       'global-script)
      ((= encd script-type)
       'room-entry-script)
      ((= excd script-type)
       'room-exit-script)
      (else (error "unknown script type")))))

(define (decode-ops decoded)
  (handle-exceptions exn
                     (cond ((eq? 'eof exn)
                            (reverse decoded))
                           (else
                            (display
                             ((condition-property-accessor 'exn
                                                           'message)
                              exn))
                            (newline)))
    (decode-ops (cons (decode-op (read-u8 current-script-port))
                      decoded))))

(define (test-run)
  (set! current-script-file
        "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201")
  (set! current-script-port (open-input-file current-script-file))
  (parse-header)
  (let print-decoded ((decoded (decode-ops '())))
    (unless (or (null? decoded)
                (equal? '(unknown shit) (car decoded)))
      (write (car decoded))
      (newline)
      (print-decoded (cdr decoded))))
  (close-input-port current-script-port)
  (set! current-script-port #f)
  (set! current-script-file #f))
