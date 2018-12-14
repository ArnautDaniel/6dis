
(defparameter *line-count* 0)

(defun aai ()
  (let ((save *line-count*))
    (incf *line-count*)
    save))

(defun convert-assembly-to-bytecodes (file output instructions)
  (with-open-file (input-data file :element-type 'unsigned-byte)
    (with-open-file (output-data output :direction :output :if-exists :supersede)
      (convert-asm-to-byte-internal input-data output-data instructions)))
  (setf *line-count* 0))

(defun convert-asm-to-byte-internal (input-data output-data instructions)
  (let ((res (read-byte input-data nil nil)))
    (when (not (equal nil res))
      (let ((poss-opcode (find-opcode-name res instructions)))
	(if poss-opcode
	    (progn
	      (format output-data "~4,'0X| ~a " *line-count* (car poss-opcode))
	      (dotimes (temp (- (cadr poss-opcode) 1))
		(format output-data "~X "  (read-byte input-data nil nil)))
	      (format output-data "~%")
	      (setf *line-count* (+ *line-count* (cadr poss-opcode)))
	      (convert-asm-to-byte-internal input-data output-data instructions))
	    (progn
	      (format output-data "~4,'0X| ~a~%" (aai) res)
	      (convert-asm-to-byte-internal input-data output-data instructions)))))))
		   
  
;;;These should be abstracted better
(defun find-opcode-name (opcode instruct-global-list)
  (car (remove-if #'null
	     (mapcar #'(lambda (x)
	      (let ((res (opcode-contained-in? opcode x)))
		(if res
		    (list (car x) res)
		    nil)))
	  instruct-global-list))))
;;; Breaking ? style for fun and danger
(defun opcode-contained-in? (opcode instruct-list)
  (let ((data (cdr instruct-list)))
    (caar (remove-if #'null
	       (mapcar #'(lambda (y)
			   (cons (car y) (member opcode (cdr y))))
		       data)
	       :key #'cadr))))

(defparameter c6502-instructions
  '((adc
     (2 #x69 #x65 #x75 #x61 #x71)
     (3 #x6d #x7d #x79))
    (and
     (2 #x29 #x25 #x35 #x21 #x31)
     (3 #x2d #x3d #x39))
    (asl
     (1 #x0a)
     (2 #x06 #x16)
     (3 #x0e #x1e))
    (bcl
     (2 #x90))
    (bcs
     (2 #xb0))
    (beq
     (2 #xf0))
    (bit
     (2 #x24)
     (3 #x2c))
    (bmi
     (2 #x30))
    (bne
     (2 #xd0))
    (bpl
     (2 #x10))
    (brk
     (1 #x00))
    (bvc
     (2 #x50))
    (bvs
     (2 #x70))
    (clc
     (1 #x18))
    (cld
     (1 #xd8))
    (cli
     (1 #x58))
    (clv
     (1 #xb8))
    (cmp
     (2 #xc9 #xc5 #xd5 #xc1 #xd1)
     (3 #xcd #xdd #xd9))
    (cpx
     (2 #xe0 #xe4)
     (3 #xec))
    (cpy
     (2 #xc0 #xc4)
     (3 #xcc))
    (dec
     (2 #xc6 #xd6)
     (3 #xce #xde))
    (dex
     (1 #xca))
    (dey
     (1 #x88))
    (eor
     (2 #x49 #x45 #x55 #x41 #x51)
     (3 #x4d #x5d #x59))
    (inc
     (2 #xe6 #xf6)
     (3 #xee #xfe))
    (inx
     (1 #xe8))
    (iny
     (1 #xc8))
    (jmp
     (3 #x4c #x6c))
    (jsr
     (3 #x20))
    (lda
     (2 #xa9 #xa5 #xb5 #xa1 #xb1)
     (3 #xad #xbd #xb9))
    (ldx
     (2 #xa2 #xa6 #xb6)
     (3 #xae #xbe))
    (ldy
     (2 #xa0 #xa4 #xb4)
     (3 #xac #xbc))
    (lsr
     (1 #x4a)
     (2 #x46 #x56)
     (3 #x4e #x5e))
    (nop
     (1 #xea))
    (ora
     (2 #x09 #x05 #x15 #x01 #x11)
     (3 #x0d #x1d #x19))
    (pha
     (1 #x48))
    (php
     (1 #x08))
    (pla
     (1 #x68))
    (plp
     (1 #x28))
    (rot
     (1 #x2a)
     (2 #x26 #x36)
     (3 #x2e #x3e))
    (ror
     (1 #x6a)
     (2 #x66 #x76)
     (3 #x6e #x7e))
    (rts
     (1 #x60))
    (sbc
     (2 #xe9 #xe5 #xf5 #xe1 #xf1)
     (3 #xed #xfd #xf9))
    (sec
     (1 #x38))
    (sei
     (1 #x78))
    (sta
     (2 #x85 #x95 #x81 #x91)
     (3 #x8d #x9d #x99))
    (stx
     (2 #x86 #x96)
     (3 #x8e))
    (sty
     (2 #x84 #x94)
     (3 #x8c))
    (tax
     (1 #xaa))
    (tay
     (1 #xa8))
    (tsx
     (1 #xba))
    (txa
     (1 #x8a))
    (txs
     (1 #x9a))
    (tya
     (1 #x98))))
