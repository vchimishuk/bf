;;; Simple Braifuck interpreter.
;;; http://wikipedia.org/wiki/Brainfuck
;;; Author: Viacheslav Chimishuk <voice@root.ua>
;;; License: GPLv3 or later.
;;; Date: 28.01.2013

; Total memory (tape) size.
(defparameter *tape-size* 100)
; The machine memory. Memory model is circular onedimensional byte tape.
(defparameter *tape* (make-array *tape-size*))
; Pointer to the current position in memory.
(defparameter *tape-pointer* 0)
; Command handlers.
(defparameter *commands* (make-hash-table))

(defun get-current ()
  "Returns tape current cell value."
  (aref *tape* *tape-pointer*))

(defun set-current (val)
  "Sets tape current cell new value."
  (setf (aref *tape* *tape-pointer*) val))

(defun bf-next (code ip)
  "> operator: Move data pointer to the next memory cell."
  (declare (ignore code))
  (setq *tape-pointer* (mod (1+ *tape-pointer*) *tape-size*))
  (1+ ip))

(defun bf-prev (code ip)
  "< operator: Move data pointer to the prev memory cell."
  (declare (ignore code))
  (setq *tape-pointer* (mod (1- *tape-pointer*) *tape-size*))
  (1+ ip))

(defun bf-inc (code ip)
  "+ operator: Increment the byte at the data pointer."
  (declare (ignore code))
  (set-current (mod (1+ (get-current)) 256))
  (1+ ip))

(defun bf-dec (code ip)
  "- operator: Decrement the byte at the data pointer."
  (declare (ignore code))
  (set-current (mod (1- (get-current)) 256))
  (1+ ip))

(defun bf-print (code ip)
  ". operator: Output the byte at the data pointer."
  (declare (ignore code))
  (format t "~c" (code-char (get-current)))
  (1+ ip))

(defun bf-read (code ip)
  ", operator: Accept one byte of input, storing its value in the byte at the data pointer."
  (declare (ignore code))
  (set-current (char-code (read-char)))
  (1+ ip))

(defun bf-loop (code ip &optional (open-loops 0))
  "[ operator: if the byte at the data pointer is zero, then instead of moving
the instruction pointer forward to the next command, jump it forward to
the command after the matching ] command."
  (if (and (zerop open-loops) (not (zerop (get-current))))
      (1+ ip)
      (case (aref code ip)
        (#\[ (bf-loop code (1+ ip) (1+ open-loops)))
        (#\] (let ((open-loops (1- open-loops))
                   (ip (1+ ip)))
               (if (zerop open-loops)
                   ip
                   (bf-loop code ip open-loops))))
        (otherwise (bf-loop code (1+ ip) open-loops)))))

(defun bf-loop-end (code ip &optional (open-loops 0))
  "] operator: if the byte at the data pointer is nonzero, then instead of moving
the instruction pointer forward to the next command, jump it back to the command
after the matching [ command."
  (if (and (zerop open-loops) (zerop (get-current)))
      (1+ ip)
      (case (aref code ip)
        (#\] (bf-loop-end code (1- ip) (1+ open-loops)))
        (#\[ (let ((open-loops (1- open-loops)))
               (if (zerop open-loops)
                   ip
                   (bf-loop-end code (1- ip) open-loops))))
        (otherwise (bf-loop-end code (1- ip) open-loops)))))

(defun interpreter (code)
  "Run a BF program."
  (let* ((len (length code))
         (code (make-array len :initial-contents (coerce code 'list)))
         (ip 0))
    (loop while (< ip len) do
         (setf ip (funcall (gethash (aref code ip) *commands*) code ip)))))

;; Set command handlers map.
(setf (gethash #\> *commands*) #'bf-next)
(setf (gethash #\< *commands*) #'bf-prev)
(setf (gethash #\+ *commands*) #'bf-inc)
(setf (gethash #\- *commands*) #'bf-dec)
(setf (gethash #\. *commands*) #'bf-print)
(setf (gethash #\, *commands*) #'bf-read)
(setf (gethash #\[ *commands*) #'bf-loop)
(setf (gethash #\] *commands*) #'bf-loop-end)


(defun simple-hello-word ()
  "Program prints Hello World! string in the simplest way."
  (interpreter (concatenate 'string
                            "+++++++++++++++++++++++++++++++++++++++++++++"
                            "+++++++++++++++++++++++++++.+++++++++++++++++"
                            "++++++++++++.+++++++..+++.-------------------"
                            "---------------------------------------------"
                            "---------------.+++++++++++++++++++++++++++++"
                            "++++++++++++++++++++++++++.++++++++++++++++++"
                            "++++++.+++.------.--------.------------------"
                            "---------------------------------------------"
                            "----.-----------------------.")))

(defun hello-world ()
  "Hello World! program with loops."
  (interpreter (concatenate 'string
                            "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++"
                            ".>+.+++++++..+++.>++.<<+++++++++++++++.>.+++."
                            "------.--------.>+.>.")))
