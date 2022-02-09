;;;; Clameboy.lisp

(in-package #:Clameboy)


(defparameter *boot-rom-path*
  "~/Code/Common-Lisp/Clameboy/resource/dmg_boot.bin")

(defun read-boot-rom (boot-rom-path)
  (with-open-file (s boot-rom-path :element-type 'unsigned-byte)
    (loop :for byte = (read-byte s nil)
	  :while byte
	  :collect byte)))

(defclass memory ()
  ((content :initform (make-array #xFFFF :initial-element 0)
	    :accessor content)))

(defun write-to-memory (memory address value)
  (setf (aref (content memory) address)
	value))

(defun read-from-memory (memory address)
  (aref (content memory) address))

(defclass cpu ()
  ((memory :initform nil
	   :accessor memory)
   (pc :initform 0
       :accessor pc)
   (sp :initform 0
       :accessor sp)
   (af :initform 0
       :accessor af)
   (bc :initform 0
       :accessor de)
   (de :initform 0
       :accessor de)))


;; TODO
;; - implement logic allowing to access the 16-bit registers as two 8-bit registers
;; - implement aux script which reads instruction table json to generate all the boilerplate code for the instructions and allows us to remove the ugly ecase statement


(defun cpu-read-from-memory-at-pc (cpu)
  (let ((value (read-from-memory (memory cpu) (pc cpu))))
    (incf (pc cpu))
    value))

(defun cpu-tick (cpu)
  (let ((opcode (cpu-read-from-memory-at-pc cpu)))
    (format t "~& [LOG] CPU execute opcode #x~x" opcode)
    (ecase opcode
      (#x31 ;; LD SP,d16
       (let ((lsb (cpu-read-from-memory-at-pc cpu))
	     (msb (cpu-read-from-memory-at-pc cpu)))
	 (setf (sp cpu)
	       (make-16bit-number lsb msb)))))))

;; #xFE #xFF
;; -> #xFFFE (little endian)
;; -> #xFEFF (big endian)

(defun << (integer count)
  (ash integer count))

(defun >> (integer count)
  (ash integer (- count)))

(defun make-16bit-number (lsb msb)
  (logior (<< msb 8)
	  lsb))


(defclass clameboy ()
  ((memory :initform (make-instance 'memory)
	   :accessor memory)
   (cpu :initform (make-instance 'cpu)
	:accessor cpu)))

(defmethod initialize-instance :after ((clameboy clameboy) &key (boot-rom-path *boot-rom-path*))
  (with-slots (cpu memory) clameboy
    (setf (memory cpu)
	  memory)
    (load-boot-rom clameboy boot-rom-path)))

(defun load-boot-rom (clameboy boot-rom-path)
  (loop :for byte :in (read-boot-rom boot-rom-path)
	:for address :from 0
	:do (write-to-memory (memory clameboy) address byte)))

(defun emulation-step (clameboy)
  (cpu-tick (cpu clameboy)))


(let ((clameboy (make-instance 'clameboy)))
  (emulation-step clameboy)
  (emulation-step clameboy))
