;; low level

(defparameter *gpio-path* "/sys/class/gpio")

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


;; menu

(defvar *menu-position* 0)

(defun update-menu (direction)
   (cond ((eq direction 'left)
	 (incf *menu-position*))
	((eq direction 'right)
	 (decf *menu-position*)))
  (format t "~a~%" *menu-position*))


;; button

(defparameter *button-pin* 21)

(defun get-button-state ()
  (gpio-read *button-pin* 'value))


;; rotary encoder

(defparameter *lpin* 26)
(defparameter *hpin* 19)
(defvar *running* nil)

(defun encoder-to-state (last current)
  (let ((state (parse-integer
		(format nil "~a~a" last current)
		:radix 2)))
    (case state
      ((#b0001 #b0111 #b1110 #b1000) 'right)
      ((#b0010 #b1011 #b1101 #b0100) 'left)
      ((#b0000 #b0101 #b1111 #b1010) 'neutral)
      (otherwise 'neutral))))

(defparameter get-rotary-encoder-state
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

;; (defparameter get-rotary-encoder-state
;;       (lambda ()
;; 	(sleep 1) 
;; 	(nth (random 3) '(left right neutral))))


;; all

(defun process-rotary-encoder-state (direction on-change)
  (when (not (eq direction 'neutral))
    (funcall on-change direction)
    (sleep 0.01)))

(defun process-button-state (state on-update)
  (declare (ignore on-update))
  (when (= state 1)
    (format t "click~%")
    (sleep 0.5)))

(defun start ()
  (setf *running* t)
  (loop
     (if *running*
	  (progn
	    (process-rotary-encoder-state (funcall get-rotary-encoder-state)
					  #'update-menu)
	    (process-button-state (get-button-state)
				  #'update-menu))
	  (return))))

(defun stop ()
  (setf *running* nil))


;; (gpio-control *lpin* 'export)
;; (gpio-control *hpin* 'export)
;; (gpio-control *button-pin* 'export)

(start)
;; (stop)



