;;;; antipasto.scm

;;; Antipasto - Scumm Script Disassembler Prototype (version 5 scripts)
;;; Copyright (C) 2007 Andreas Scholta
;;; Time-stamp: <2007-07-05 19:06:17 brx>

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
(define current-script-offset #f)

(define opcode-register (make-hash-table))

(define (register-opcode name code handler)
  (hash-table-set! opcode-register code (cons name handler)))

(define (register-complex-opcode name codes handler)
  (for-each (cut register-opcode name <> handler) codes))

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

(define param-1 #x80)
(define param-2 #x40)
(define param-3 #x20)

(define (generate-opcodes base-code flags)
  (let gen-opcodes ((flag-combos (generate-subsets flags))
                    (accum '()))
    (if (null? flag-combos)
        accum
        (gen-opcodes (cdr flag-combos)
                     (cons (fold bior base-code (car flag-combos))
                           accum)))))

(define (register-123-op name base-code handler n)
  (when (between? n 0 3)
    (for-each (cut register-opcode name <> handler)
              (generate-opcodes base-code
                                (take (list param-1 param-2 param-3) n)))))

(define (fetch-byte)
  (set! current-script-offset (+ current-script-offset 1))
  (read-u8 current-script-port))

(define (fetch-word)
  (set! current-script-offset (+ current-script-offset 2))
  (read-le-u16 current-script-port))

(define (get-var)
  (define (get-num-sym i)
    (cond ((not (zero? (band i #x8000)))
           (if (>= (band i #xfff) #x800)
               '??bit??
               'bit))
          ((not (zero? (band i #x4000)))
           (if (>= (band i #xfff) #x10)
               '??local??
               'local))
          (else
           (if (>= (band i #xfff) #x320)
               '??var??
               'var))))
  (let ((i (fetch-word)))
    (list (get-num-sym i)
          (if (zero? (band i #x2000))
              (band i #xfff)
              (list '+
                    (band i #xfff)
                    (let ((j (fetch-word)))
                      (if (zero? (band j #x2000))
                          (band j #xfff)
                          (list (get-num-sym (bxor j #x2000))
                                (band j #xfff)))))))))

(define (get-var/byte op mask)
  (if (zero? (band op mask))
      (fetch-byte)
      (get-var)))

(define (get-var/word op mask)
  (if (zero? (band op mask))
      (fetch-word)
      (get-var)))

(define suck-v (compose list (hole get-var)))

(define suck-vb (compose list (cut get-var/byte <> param-1)))
(define suck-vw (compose list (cut get-var/word <> param-1)))

(define (suck-vb-alist op)
  (list (get-var/byte op param-1)
        (get-arg-list)))

(define (suck-vw-alist op)
  (list (get-var/word op param-1)
        (get-arg-list)))

(define (suck-vb-vb op)
  (list (get-var/byte op param-1)
        (get-var/byte op param-2)))

(define (suck-vb-vw op)
  (list (get-var/byte op param-1)
        (get-var/word op param-2)))

(define (suck-vw-vb op)
  (list (get-var/word op param-1)
        (get-var/byte op param-2)))

(define (suck-vw-vw op)
  (list (get-var/word op param-1)
        (get-var/word op param-2)))

(define (suck-vw-vb-alist op)
  (list (get-var/word op param-1)
        (get-var/byte op param-2)
        (get-arg-list)))

(define (suck-vb-vb-vb op)
  (list (get-var/byte op param-1)
        (get-var/byte op param-2)
        (get-var/byte op param-3)))

(define (suck-vb-vb-vw op)
  (list (get-var/byte op param-1)
        (get-var/byte op param-2)
        (get-var/word op param-3)))

(define (suck-vb-vw-vw op)
  (list (get-var/byte op param-1)
        (get-var/word op param-2)
        (get-var/word op param-3)))

(define (suck-vw-vw-vw op)
  (list (get-var/word op param-1)
        (get-var/word op param-2)
        (get-var/word op param-3)))

(define (suck-vw-vw-vb op)
  (list (get-var/word op param-1)
        (get-var/word op param-2)
        (get-var/byte op param-3)))

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
         ((0) (cons "Pos" (suck-vw-vw byte)))
         ((1) (cons "Color" (suck-vb byte)))
         ((2) (cons "Clipped" (suck-vw byte)))
         ((3) (cons "RestoreBG" (suck-vw-vw byte)))
         ((4) '("Center"))
         ((6) '("Left"))
         ((7) '("Overhead"))
         ((8) (cons "PlayCDTrack" (suck-vw-vw byte)))
         ((15) (signal (cons #xff (list "Text" (get-ascii)))))
         (else (error "printEgo fucked up")))))))

(register-123-op "actorFollowCamera" #x52 suck-vb 1)
(register-123-op "animateCostume" #x11 suck-vb-vb 2)
(register-123-op "putActor" #x01 suck-vb-vb-vw 3)
(register-123-op "putActorInRoom" #x2d suck-vb-vb 2)
(register-123-op "faceActor" #x09 suck-vb-vw 2)

(register-123-op "findInventory" #x3d suck-vb-vb 2)
(register-123-op "findObject" #x35 suck-vb-vb 2)
(register-123-op "freezeScripts" #x60 suck-vb 1)

(register-opcode "cutscene" #x40 (compose list (hole get-arg-list)))

(register-opcode "override" #x58
                 (lambda (_)
                   (list (if (zero? (fetch-byte))
                             'end
                             'begin))))

;; misses convertTable hack
(define (handle-actor-ops op)
  (list (get-var/byte param-1 op)
        (process-bytes-from-script
         (cut = #xff <>)
         (lambda (byte)
           (let ((b (band byte #x1f)))
             (case b
               ((0) (cons "Unknown" (suck-vb byte)))
               ((1) (cons "Costume" (suck-vb byte)))
               ((2) (cons "WalkSpeed" (suck-vb-vb byte)))
               ((3) (cons "Sound" (suck-vb byte)))
               ((4) (cons "WalkAnimNr" (suck-vb byte)))
               ((5) (cons "TalkAnimNr" (suck-vb byte)))
               ((6) (cons "StandAnimNr" (suck-vb byte)))
               ((7) (cons "Nothing" (suck-vb-vb-vb byte)))
               ((8) '("Init"))
               ((9) (cons "Elevation" (suck-vw byte)))
               ((10) (list "DefaultAnims" 0))
               ((11) (cons "Palette" (suck-vb-vb byte)))
               ((12) (cons "TalkColor" (suck-vb byte)))
               ((13) (list "Name" (get-ascii)))
               ((14) (cons "InitAnimNr" (suck-vb byte)))
               ((16) (cons "Width" (suck-vb byte)))
               ((17) (cons "Scale" (suck-vb-vb byte)))
               ((18) '("NeverZClip"))
               ((19) (cons "AlwaysZClip" (suck-vb byte)))
               ((20) '("IgnoreBoxes"))
               ((21) '("FollowBoxes"))
               ((22) (cons "AnimSpeed" (suck-vb byte)))
               (else (error "actorOps fucked up"))))))))

(register-123-op "actorOps" #x13 handle-actor-ops 2)

(register-123-op "loadRoom" #x72 suck-vb 1)

(define (handle-room-ops)
  (let* ((byte (fetch-byte))
         (b (band byte #x1f)))
    (case b
      ((#x01) (cons "RoomScroll" (suck-vw-vw byte)))
      ((#x02) '("RoomColor"))
      ((#x03) (cons "SetScreen" (suck-vw-vw byte)))
      ((#x04) (cons "SetPalColor"
                    (append (suck-vw-vw-vw byte)
                            (suck-vb (fetch-byte)))))
      ((#x05) '("ShakeOn"))
      ((#x06) '("ShakeOff"))
      ((#x07) '("Unused"))
      ((#x08) (cons "RoomIntensity"
                    (suck-vb-vb-vb byte)))
      ((#x09) (cons "saveLoad?" (suck-vb-vb byte)))
      ((#x0a) (cons "screenEffect?" (suck-vw byte)))
      ((#x0b) (cons "setRGBRoomIntensity"
                    (append (suck-vw-vw-vw byte)
                            (suck-vb-vb (fetch-byte)))))
      ((#x0c) (cons "setRoomShadow"
                    (append (suck-vw-vw-vw byte)
                            (suck-vb-vb (fetch-byte)))))
      ((#x0d) (list "saveString"
                    (get-var/byte byte param-1)
                    (get-ascii)))
      ((#x0e) (list "loadString"
                    (get-var/byte byte param-1)
                    (get-ascii)))
      ((#x0f) (cons "palManipulate"
                    (append (suck-vb byte)
                            (suck-vb-vb (fetch-byte))
                            (suck-vb (fetch-byte)))))
      ((#x10) (cons "colorCycleDelay"
                    (suck-vb-vb byte)))
      (else (error "Unknown roomOp")))))

(register-123-op "roomOps" #x33 (hole handle-room-ops) 2)

(define (handle-verb-ops op)
  (list (get-var/byte op param-1)
        (process-bytes-from-script
         (cut = #xff <>)
         (lambda (byte)
           (let ((b (band byte #x1f)))
             (case b
               ((#x01) (cons "Image" (suck-vw byte)))
               ((#x02) (list "Text" (get-ascii)))
               ((#x03) (cons "Color" (suck-vb byte)))
               ((#x04) (cons "HiColor" (suck-vb byte)))
               ((#x05) (cons "SetXY" (suck-vw-vw byte)))
               ((#x06) '("On"))
               ((#x07) '("Off"))
               ((#x08) '("Delete"))
               ((#x09) '("New"))
               ((#x10) (cons "DimColor" (suck-vb byte)))
               ((#x11) '("Dim"))
               ((#x12) (cons "Key" (suck-vb byte)))
               ((#x13) '("Center"))
               ((#x14) (cons "SetToString" (suck-vw byte)))
               ((#x16) (cons "SetToObject" (suck-vw-vb byte)))
               ((#x17) (cons "BackColor" (suck-vb byte)))
               (else (error "Unknown verbOp"))))))))

(register-123-op "verbOps" #x7a handle-verb-ops 1)

(define (handle-cursor-command)
  (list (let* ((byte (fetch-byte))
               (b (band byte #x1f)))
          (case b
            ((#x01) '("CursorShow"))
            ((#x02) '("CursorHide"))
            ((#x03) '("UserputOn"))
            ((#x04) '("UserputOff"))
            ((#x05) '("CursorSoftOn"))
            ((#x06) '("CursorSoftOff"))
            ((#x07) '("UserputSoftOn"))
            ((#x08) '("UserputSoftOff"))
            ((#x0a) (cons "SetCursorImg" (suck-vb-vb byte)))
            ((#x0b) (cons "SetCursorHotspot" (suck-vb-vb-vb byte)))
            ((#x0c) (cons "InitCursor" (suck-vb byte)))
            ((#x0d) (cons "InitCharset" (suck-vb byte)))
            ((#x0e) (list "CursorCommand" (get-arg-list)))
            (else (error "Unknown cursor command"))))))

(register-opcode "cursorCmd" #x2c (hole handle-cursor-command))

(register-complex-opcode "doSentence"
                         '(#x19 #x39 #x59 #x79 #x99 #xb9 #xd9 #xf9)
                         (lambda (op)
                           (let ((verb (get-var/byte op param-1)))
                             (if (= #xfe verb)
                                 (list 'stop)
                                 (list verb
                                       (get-var/word op param-2)
                                       (get-var/word op param-3))))))

(register-opcode "breakHere" #x80 (constantly '()))
(register-opcode "endCutscene" #xc0 (constantly '()))

(register-complex-opcode "stopObjectCode" '(#x00 #xa0) (constantly '()))

(register-123-op "print" #x14
                 (lambda (op)
                   (list (get-var/byte op param-1)
                         (decode-parse-string)))
                 1)

(register-opcode "printEgo" #xd8 (compose list (hole decode-parse-string)))

(define (handle-start-script op)
  (list (get-var/byte op param-1)
        (get-arg-list)))

(register-complex-opcode "startScript"
                         '(#x0a #x2a #x4a #x6a #x8a #xaa #xca #xea)
                         handle-start-script)

(register-123-op "chainScript" #x42 suck-vb 1)

(register-123-op "debug" #x6b suck-vw 1)

(register-opcode "delayVariable" #x2b suck-v)

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

(register-123-op "setClass"
                 #x5d
                 (lambda (op)
                   (list (get-var/word op param-1)
                         (get-arg-list)))
                 1)

(register-123-op "setObjectName"
                 #x54
                 (lambda (op)
                   (list (get-var/word op param-1)
                         (get-ascii)))
                 1)

(register-123-op "drawObject"
                 #x05
                 (lambda (op)
                   (cons (get-var/word op param-1)
                         (let ((byte (fetch-byte)))
                           (cond
                             ((= (band byte #x1f) 1)
                              (cons "setXY" (suck-vw-vw byte)))
                             ((= (band byte #x1f) 2)
                              (cons "setImage" (suck-vw byte)))
                             (else '())))))
                 2)

(register-123-op "drawBox"
                 #x3f
                 (lambda (op)
                   (append (suck-vw-vw op)
                           (suck-vw-vw-vb (fetch-byte))))
                 2)

(register-123-op "startSound" #x1c suck-vb 1)
(register-123-op "stopSound" #x3c suck-vb 1)

(register-123-op "setState" #x7 suck-vw-vb 2)

(register-opcode "soundKludge" #x4c (compose list (hole get-arg-list)))

(register-123-op "lights"
                 #x70
                 (lambda (op)
                   (list (get-var/byte op param-1)
                         (fetch-byte)
                         (fetch-byte)))
                 1)

(register-123-op "loadRoomWithEgo"
                 #x24
                 (lambda (op)
                   (append (suck-vw-vb op)
                           (list (fetch-word)
                                 (fetch-word))))
                 2)

;; game version check missing
(define (handle-matrix-ops op)
  (let* ((byte (fetch-byte))
         (b (band #x1f byte)))
    (case b
      ((1) (cons "setBoxFlags" (suck-vb-vb byte)))
      ((2) (cons "setBoxScale" (suck-vb-vb byte)))
      ((3) (cons "setBoxSlot" (suck-vb-vb byte)))
      ((4) '("createBoxMatrix"))
      (else (error "Unknown matrix op")))))

(register-123-op "matrixOps" #x30 handle-matrix-ops 1)

(register-123-op "oldRoomEffect"
                 #x5c
                 (lambda (op)
                   (if (= 3 (band #x1f (fetch-byte)))
                       (cons 'set (suck-vw op))
                       (cons 'fadein (suck-vw op))))
                 1)

(register-123-op "panCameraTo" #x12 suck-vb 1)
(register-123-op "pickupObject" #x25 suck-vw-vb 2)
(register-123-op "pickupObjectOld" #x50 suck-vw 1)

(register-123-op "putActorAtObject" #x0e suck-vb-vw 2)

;; script version missing
(define (handle-resource-routines)
  (let* ((op (fetch-byte))
         (subop (band op #x1f)))
    (case subop
      ((#x1) (cons "loadScript" (suck-vb op)))
      ((#x2) (cons "loadSound" (suck-vb op)))
      ((#x3) (cons "loadCostume" (suck-vb op)))
      ((#x4) (cons "loadRoom" (suck-vb op)))
      ((#x5) (cons "nukeScript" (suck-vb op)))
      ((#x6) (cons "nukeSound" (suck-vb op)))
      ((#x7) (cons "nukeCostume" (suck-vb op)))
      ((#x8) (cons "nukeRoom" (suck-vb op)))
      ((#x9) (cons "lockScript" (suck-vb op)))
      ((#xa) (cons "lockSound" (suck-vb op)))
      ((#xb) (cons "lockCostume" (suck-vb op)))
      ((#xc) (cons "lockRoom" (suck-vb op)))
      ((#xd) (cons "unlockScript" (suck-vb op)))
      ((#xe) (cons "unlockSound" (suck-vb op)))
      ((#xf) (cons "unlockCostume" (suck-vb op)))
      ((#x10) (cons "unlockRoom" (suck-vb op)))
      ((#x11) '("clearHeap"))
      ((#x12) (cons "loadCharset" (suck-vb op)))
      ((#x13) (cons "nukeCharset" (suck-vb op)))
      ((#x14) (cons "loadFlObject" (suck-vb-vw op)))
      ((#x23) (cons "resUnk1" (suck-vb-vb op)))
      ((#x24) (cons "resUnk2" (append (suck-vb-vb op)
                                      (fetch-byte))))
      ((#x25) (cons "resUnk3" (suck-vb-vb op)))
      (else (error "Unknown resource routine")))))

(register-123-op "resourceRoutines" #x0c (hole handle-resource-routines) 1)

(define (handle-save-load-vars)
  (let ((byte (fetch-byte)))
    (cons (if (= byte 1)
              "Save"
              "Load")
          (process-bytes-from-script
           zero?
           (lambda (byte)
             (let ((b (band byte #x1f)))
               (case b
                 ((#x01) (list "VarRange" (get-var) (get-var)))
                 ((#x02) (cons "StringRange" (suck-vb-vb byte)))
                 ((#x03) (list "Open" (get-ascii)))
                 ((#x04) "Append")
                 ((#x1f) "Close"))))))))

(register-opcode "saveLoadVars" #xa7 (hole handle-save-load-vars))

(define (handle-save-restore-verbs)
  (let ((byte (fetch-byte)))
    (cons (case byte
            ((1) "saveVerbs")
            ((2) "restoreVerbs")
            ((3) "deleteVerbs")
            (else (error "Unknown saveRestoreVerbs subop")))
          (suck-vb-vb-vb byte))))

(register-opcode "saveRestoreVerbs" #xab (hole handle-save-restore-verbs))

(define (handle-pseudoroom)
  (let ((i (fetch-byte)))
    (cons i
          (process-bytes-from-script
           zero?
           (lambda (j)
             (if (zero? (band j #x80))
                 'ignored
                 (band j #x7f)))))))

(register-opcode "pseudoRoom" #xcc (hole handle-pseudoroom))

(register-123-op "setCameraAt" #x32 suck-vw 1)
(register-123-op "setOwnerOf" #x29 suck-vw-vb 2)

(register-123-op "setVarRange"
                 #x26
                 (lambda (op)
                   (cons (get-var)
                         (let ((i (fetch-byte))
                               (fetch (if (band op #x80)
                                          fetch-word
                                          fetch-byte)))
                           (list i
                                 (let accumulate ((i i)
                                                  (acc '()))
                                   (if (zero? i)
                                       (reverse acc)
                                       (accumulate (sub1 i)
                                                   (cons (fetch)
                                                         acc))))))))
                 1)

(register-123-op "startMusic" #x02 suck-vb 1)
(register-123-op "startObject" #x37 suck-vw-vb-alist 2)

(register-opcode "stopMusic" #x20 (constantly '()))

(register-123-op "stopObjectScript" #x6e suck-vw 1)
(register-123-op "stopScript" #x62 suck-vb 1)

(define (handle-string-ops)
  (let* ((byte (fetch-byte))
         (b (band byte #x1f)))
    (case b
      ((1) (cons "PutCodeInString" (append (suck-vb byte) (get-ascii))))
      ((2) (cons "CopyToString" (suck-vb-vb byte)))
      ((3) (cons "SetStringChar" (suck-vb-vb-vb byte)))
      ((4) (cons* "GetStringChar" (get-var) (suck-vb-vb byte)))
      ((5) (cons "CreateString" (suck-vb-vb byte)))
      (else (error "Unknown string op")))))

(register-opcode "stringOps" #x27 (hole handle-string-ops))

(register-opcode "systemOps"
                 #x98
                 (lambda (_)
                   (list (let ((b (fetch-byte)))
                           (case b
                             ((1) 'restart)
                             ((2) 'pause)
                             ((3) 'quit)
                             (else (error "Unknown system op")))))))

(register-123-op "walkActorTo" #x1e suck-vb-vw-vw 3)
(register-123-op "walkActorToObject" #x36 suck-vb-vw 2)
(register-123-op "walkActorToActor"
                 #x0d
                 (compose (cut append <> (list (fetch-byte)))
                          suck-vb-vb)
                 2)

(define (register-complex-set set op fetch-set-params n)
  (register-123-op 'set!
                   op
                   (lambda (op)
                     (list (get-var)
                           (cons set (fetch-set-params op))))
                   n))

(register-complex-set "actorFromPos" #x15 suck-vw-vw 2)
(register-complex-set "getDist" #x34 suck-vw-vw 2)
(register-complex-set "getVerbEntrypoint" #x0b suck-vw-vw 2)

(define (register-simple-set set op
                             #!optional (fetch-set-param suck-vb))
  (register-complex-set set op fetch-set-param 1))

(register-simple-set "getRandomNr" #x16)
(register-simple-set "isScriptRunning" #x68)
(register-simple-set "isSoundRunning" #x7c)
(register-simple-set "getActorCostume" #x71)
(register-simple-set "getActorElevation" #x06)
(register-simple-set "getActorFacing" #x63)
(register-simple-set "getActorMoving" #x56)
(register-simple-set "getActorRoom" #x03)
(register-simple-set "getActorScale" #x3b)
(register-simple-set "getActorWalkBox" #x7b)
(register-simple-set "getActorWidth" #x6c)

(register-simple-set "getActorX" #x43 suck-vw)  ;indy3 hack missing
(register-simple-set "getActorY" #x23 suck-vw)  ;indy3 hack missing

(register-simple-set "getAnimCounter" #x22)
(register-simple-set "getClosestObjActor" #x66 suck-vw)
(register-simple-set "getInventoryCount" #x31)
(register-simple-set "getObjectOwner" #x10)
(register-simple-set "getObjectState" #x0f suck-vw)     ;small header missing
(register-simple-set "getStringWidth" #x67)

;; o5_move
(register-simple-set 'identity #x1a suck-vw)

(register-opcode 'inc! #x46 (compose list (hole get-var)))
(register-opcode 'dec! #xc6 (compose list (hole get-var)))

(define (handle-sarith op)
  (cons (get-var) (suck-vw op)))

(register-123-op 'inc! #x5a handle-sarith 1)
(register-123-op 'dec! #x3a handle-sarith 1)
(register-123-op 'mul! #x1b handle-sarith 1)
(register-123-op 'div! #x5b handle-sarith 1)

(register-123-op 'bor! #x57 handle-sarith 1)
(register-123-op 'band! #x17 handle-sarith 1)

(define (calc-abs-jump relative)
  (sprintf "~X" ;only for testing purposes with intermediary format
           (band #x7fff (+ relative current-script-offset))))

(register-opcode 'goto
                 #x18
                 (lambda (_)
                   (list (calc-abs-jump (fetch-word)))))

(define (register-simple-cond-jump pred op)
  (register-opcode 'goto-unless
                   op
                   (lambda (_)
                     (let ((var (get-var)))
                       (list (calc-abs-jump (fetch-word))
                             (list pred var))))))

(register-simple-cond-jump 'not-zero? #xa8)
(register-simple-cond-jump 'zero? #x28)

(define (register-binary-cond-jump bpred op)
  (register-123-op 'goto-unless
                   op
                   (lambda (op)
                     (let ((a (get-var))
                           (b (get-var/word op param-1)))
                       (list (calc-abs-jump (fetch-word))
                             (list bpred b a))))
                   1))

(register-binary-cond-jump '<= #x38)
(register-binary-cond-jump '< #x44)
(register-binary-cond-jump '>= #x4)
(register-binary-cond-jump '> #x78)
(register-binary-cond-jump '/= #x8)
(register-binary-cond-jump '= #x48)

(define (make-if-handler name fetcher)
  (lambda (op)
    (let ((args (fetcher op)))
      (list (calc-abs-jump (fetch-word))
            (cons name args)))))

(register-123-op 'goto-unless
                 #x1d
                 (make-if-handler "classOfIs" suck-vw-alist)
                 1)

(register-123-op 'goto-unless
                 #x2f
                 (make-if-handler "ifNotState" suck-vw-vb)
                 2)

(register-123-op 'goto-unless
                 #x4f
                 (make-if-handler "ifState" suck-vw-vb)
                 2)

(register-123-op 'goto-unless
                 #x1f
                 (make-if-handler "isActorInBox" suck-vb-vb)
                 2)

(define (handle-expression)
  (list
   (get-var)
   (process-bytes-from-script
    (cut = #xff <>)
    (lambda (byte)
      (let ((b (band byte #x1f)))
        (case b
          ((1) (get-var/word byte param-1))
          ((2) '+)
          ((3) '-)
          ((4) '*)
          ((5) '/)
          ((6) (cddr (decode-op (fetch-byte))))))))))

(register-opcode 'set! #xac (hole handle-expression))

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
       (set-file-position! current-script-port 8))
      ((= encd script-type)
       (set-file-position! current-script-port 8))
      ((= excd script-type)
       (set-file-position! current-script-port 8))
      ((= verb script-type)
       (error "VERB script header skipping not yet implemented"))
      (else (error "unknown script type")))))

(define (decode-ops decoded)
  (handle-exceptions exn (and (eq? 'eof exn) (reverse decoded))
                     (let ((decoded-op (decode-op (fetch-byte))))
                       (if decoded-op
                           (decode-ops (cons decoded-op decoded))
                           (reverse decoded)))))

(define (test-run)
  (set! current-script-file
;; "/home/brx/code/gsoc2007-decompiler/M1.scummV5/81.cu_bar_2.0092"
"/home/brx/code/gsoc2007-decompiler/M2.scummV5/entry-4.dmp"
;; "/home/brx/code/gsoc2007-decompiler/M2.scummV5/room-15-203.dmp";
;; "/home/brx/code/gsoc2007-decompiler/M1.scummV5/01.beach.0201"
        )
  (set! current-script-port (open-input-file current-script-file))
  (set! current-script-offset 0)
  (parse-header)
  (let print-decoded ((decoded (decode-ops '())))
    (unless (or (null? decoded)
                (not decoded)
                (not (car decoded)))
      (printf "[~X] (~X) "
              (caar decoded)
              (cadar decoded))
      (write (cddar decoded))
      (newline)
      (print-decoded (cdr decoded))))
  (close-input-port current-script-port)
  (set! current-script-port #f)
  (set! current-script-file #f)
  (set! current-script-offset #f))
