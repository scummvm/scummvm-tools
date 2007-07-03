;;;; antipasto.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-03 03:56:49 brx>

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

(define (process-bytes-from-script finished? mp
                                   #!optional (acc cons)
                                              (finally reverse)
                                              (nil '()))
  (let fetch-bytes ((byte (fetch-byte))
                    (accum nil))
    (if (finished? byte)
        (finally accum)
        (condition-case
            (let ((val (mp byte)))
              (fetch-bytes (fetch-byte) (acc val accum)))
          (e (exn) (signal e))
          (var () (fetch-bytes (car var)
                               (cons (cdr var) accum)))))))

(define (get-arg-list)
  (process-bytes-from-script
   (cut = #xff <>)
   (cut get-var/word <> param-1)))

(define (get-ascii)
  (process-bytes-from-script
   zero?
   (lambda (byte)
     (if (= byte #xff)
         (let ((a (fetch-byte)))
           (if (and (/= a 1) (/= a 2)
                    (/= a 3) (/= a 8))
               (list byte a (fetch-byte) (fetch-byte))
               (list byte a)))
         (list byte)))
   (lambda (x z) (append z x))
   (compose list->string (cut map integer->char <>))))

(define (decode-parse-string)
  (process-bytes-from-script
   (cut = #xff <>)
   (lambda (byte)
     (let ((b (band byte #xf)))
       (case b
         ((0) (list "Pos"
                    (get-var/word byte param-1)
                    (get-var/word byte param-2)))
         ((1) (list "Color" (get-var/byte byte param-1)))
         ((2) (list "Clipped" (get-var/word byte param-1)))
         ((3) (list "RestoreBG"
                    (get-var/word byte param-1)
                    (get-var/word byte param-2)))
         ((4) "Center")
         ((6) "Left")
         ((7) "Overhead")
         ((8) (list "PlayCDTrack"
                    (get-var/word byte param-1)
                    (get-var/word byte param-2)))
         ((15) (signal (cons #xff (list "Text" (get-ascii)))))
         (else (error "printEgo fucked up")))))))

(make-123-op "actorFollowCamera"
             #x52
             (compose list (cut get-var/byte <> param-1))
             1)

(register-opcode "cutscene" #x40 (compose list (hole get-arg-list)))

(make-123-op "animateCostume"
             #x11
             (lambda (op)
               (list (get-var/byte op param-1)
                     (get-var/byte op param-2)))
             2)

;; misses convertTable hack
(define (handle-actor-ops op)
  (cons (get-var/byte param-1 op)
        (process-bytes-from-script
         (cut = #xff <>)
         (lambda (byte)
           (let ((b (band byte #x1f)))
             (case b
               ((0) (list "Unknown" (get-var/byte byte param-1)))
               ((1) (list "Costume" (get-var/byte byte param-1)))
               ((2) (list "WalkSpeed"
                          (get-var/byte byte param-1)
                          (get-var/byte byte param-2)))
               ((3) (list "Sound" (get-var/byte byte param-1)))
               ((4) (list "WalkAnimNr" (get-var/byte byte param-1)))
               ((5) (list "TalkAnimNr"
                          (get-var/byte byte param-1)
                          (get-var/byte byte param-2)))
               ((6) (list "StandAnimNr"
                          (get-var/byte byte param-1)))
               ((7) (list "Nothing"
                          (get-var/byte byte param-1)
                          (get-var/byte byte param-2)
                          (get-var/byte byte param-3)))
               ((8) (list "Init" 0))
               ((9) (list "Elevation" (get-var/word byte param-1)))
               ((10) (list "DefaultAnims" 0))
               ((11) (list "Palette"
                           (get-var/byte byte param-1)
                           (get-var/byte byte param-2)))
               ((12) (list "TalkColor" (get-var/byte byte param-1)))
               ((13) (list "Name" "uuuuh..."))
               ((14) (list "InitAnimNr" (get-var/byte byte param-1)))
               ((16) (list "Width" (get-var/byte byte param-1)))
               ((17) (list "Scale"
                           (get-var/byte byte param-1)
                           (get-var/byte byte param-2)))
               ((18) (list "NeverZClip" 0))
               ((19) (list "AlwaysZClip" (get-var/byte byte param-1)))
               ((20) (list "IgnoreBoxes" 0))
               ((21) (list "FollowBoxes" 0))
               ((22) (list "AnimSpeed" (get-var/byte byte param-1)))
               (else (error "actorOps fucked up"))))))))

(register-opcode "actorOps" #x13 handle-actor-ops)
(register-opcode "actorOps" #x53 handle-actor-ops)
(register-opcode "actorOps" #x93 handle-actor-ops)
(register-opcode "actorOps" #xd3 handle-actor-ops)

(register-opcode "breakHere" #x80 (constantly '()))
(register-opcode "endCutscene" #xc0 (constantly '()))

(register-opcode "stopObjectCode" #x00 (constantly '()))
(register-opcode "stopObjectCode" #xa0 (constantly '()))

(register-opcode "printEgo" #xd8 (compose list (hole decode-parse-string)))

(define (handle-start-script op)
  (list (get-var/byte op param-1)
        (get-arg-list)))

(register-opcode "startScript" #x0a handle-start-script)
(register-opcode "startScript" #x2a handle-start-script)
(register-opcode "startScript" #x4a handle-start-script)
(register-opcode "startScript" #x6a handle-start-script)
(register-opcode "startScript" #x8a handle-start-script)
(register-opcode "startScript" #xaa handle-start-script)
(register-opcode "startScript" #xca handle-start-script)
(register-opcode "startScript" #xea handle-start-script)

(register-opcode "wait"
                 #xae
                 (lambda (_)
                   (let ((byte (fetch-byte)))
                     (case byte
                       ((1 81) (cons 'actor
                                     (get-var/byte byte param-1)))
                       ((2) (list 'message))
                       ((3) (list 'camera))
                       ((4) (list 'sentence))
                       (else (list 'weekend))))))

(register-opcode "delay"
                 #x2e
                 (lambda (_)
                   (list (bior (fetch-byte)
                               (ash (fetch-byte) 8)
                               (ash (fetch-byte) 16)))))

(define (register-simple-set op set)
  (make-123-op "set"
               op
               (lambda (op)
                 (list (get-var)
                       (list set
                             (get-var/byte op param-1))))
               1))

(register-simple-set #x68 "isScriptRunning")
(register-simple-set #x71 "getActorCostume")

(register-opcode "goto"
                 #x18
                 (lambda (_) (list (fetch-word))))

(define (register-simple-cond-jump op pred)
  (register-opcode "goto-if"
                   op
                   (lambda (_)
                     (let ((var (get-var)))
                       (list (fetch-word)
                             (list pred var))))))

(register-simple-cond-jump #xa8 'not-zero?)
(register-simple-cond-jump #x28 'zero?)

(define (register-binary-cond-jump op bpred)
  (make-123-op "goto-if"
               op
               (lambda (op)
                 (let ((a (get-var))
                       (b (get-var/word op param-1)))
                   (list (fetch-word)
                         (list bpred a b))))
               1))

(register-binary-cond-jump #x38 '<=)
(register-binary-cond-jump #x44 '<)
(register-binary-cond-jump #x4 '>=)
(register-binary-cond-jump #x78 '>)
(register-binary-cond-jump #x8 '!=)
(register-binary-cond-jump #x48 '==)

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
