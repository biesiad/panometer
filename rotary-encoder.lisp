(defun read-gpio-pin (pin)
  ;;  (nth (random 4) '("00" "01" "11" "10")))
  (with-open-file (file (format nil "/sys/class/gpio/gpio~d/value" pin)
			:direction :input
			:element-type 'unsigned-byte)
    (read-byte file)))

(defun write-gpio-pin (pin)
  (with-open-file (file (format nil "/sys/class/gpio/gpio~d/value" pin)
			:direction :output
			:element-type 'unsigned-byte)
    (write-byte file)))



(defun encoder-to-state (last current)
  (let ((state (parse-integer
		(format nil "~a~a" last current)
		:radix 2)))
    (case state
      ((#b0001 #b0111 #b1110 #b1000) 'left)
      ((#b0010 #b1011 #b1101 #b0100) 'right)
      ((#b0000 #b0101 #b1111 #b1010) 'neutral)
      (otherwise (error (format nil "Invalid encoder state: ~4,'0b" state))))))

(defun make-get-position ()
  (let ((last-value nil))
    (lambda ()
      (let ((current-value (read-gpio-pin 26)))
	(if (null last-value)
	    (progn
	      (setf last-value current-value)
	      'neutral)
	    (encoder-to-state last-value current-value))))))

(let ((get-position (make-get-position)))
  (print (funcall get-position))
  (print (funcall get-position))
  (print (funcall get-position)))


(defvar *cursor* 0)

(defvar *menu-length* 10)

(defun inc-cursor ()
  (when (< *cursor* (- *menu-length* 1))
    (incf *cursor*)))

(defun dec-cursor ()
  (when (> *cursor* 0)
    (decf *cursor*)))

(defun process ()
  (case (get-position)
    (left
     (inc-cursor)
     (print *cursor*))
    (right
     (dec-cursor)
     (print *cursor*)))
  (sleep 1))

(defparameter *running* nil)

(defun stop () (setf *running* nil))

(defun start ()
  (setf *running* t)
  (loop
    (if *running*
	(process)
	(return))))

(start)

(stop)
