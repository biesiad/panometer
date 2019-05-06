(defparameter *gpio-path* "/sys/class/gpio")

(defparameter *lpin* 10)
(defparameter *hpin* 16)

(defun gpio-control (pin action)
  (with-open-file (file (format nil "~a/~a" *gpio-path* (string-downcase action))
			:direction :output
			:if-exists :overwrite)
    (format file "~d" pin)))

(defun gpio-read (pin attribute)
  (with-open-file (file (format nil "~a/gpio~d/~a" *gpio-path* pin (string-downcase attribute))
			:direction :input)
    (parse-integer (read-line file))))

(defun gpio-write (pin attribute value)
  (with-open-file (file (format nil "~a/gpio~d/~a" *gpio-path* pin (string-downcase attribute))
			:direction :output
			:element-type 'unsigned-byte)
    (format file "~a" value)))


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
      (let ((current-value (format nil "~a~a"
				    (gpio-read *lpin* 'value)
				    (gpio-read *hpin* 'value))))
	(if (null last-value)
	    (progn
	      (setf last-value current-value)
	      'neutral)
	    (encoder-to-state last-value current-value))))))


(defvar *cursor* 0)

(defvar *menu-length* 10)

(defun cursor-inc ()
  (when (< *cursor* (- *menu-length* 1))
    (incf *cursor*)))

(defun cursor-dec ()
  (when (> *cursor* 0)
    (decf *cursor*)))

(defun process ()
  (let ((get-position (make-get-position)))
    (case (funcall get-position)
      (left
       (cursor-inc)
       (print *cursor*))
      (right
       (cursor-dec)
       (print *cursor*)))
    (sleep 1)))


(defvar *running* nil)

(defun stop () (setf *running* nil))

(defun start ()
  (setf *running* t)
  (loop
    (if *running*
	(process)
	(return))))


;; (gpio-control 16 'export)
;; (gpio-control 10 'export)

(start)
(stop)
