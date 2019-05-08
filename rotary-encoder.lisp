;; menu

(defvar *menu-position* 0)

(defun update-menu (direction)
   (cond ((eq direction 'left)
	 (incf *menu-position*))
	((eq direction 'right)
	 (decf *menu-position*))))


;; rotary encoder

(defparameter *gpio-path* "/sys/class/gpio")
(defparameter *lpin* 26)
(defparameter *hpin* 19)
(defvar *running* nil)

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
      ((#b0001 #b0111 #b1110 #b1000) 'right)
      ((#b0010 #b1011 #b1101 #b0100) 'left)
      ((#b0000 #b0101 #b1111 #b1010) 'neutral)
      (otherwise 'neutral))))
;;      (otherwise (progn (format t "ERROR: ~4,'0b~%" state) 'neutral)))))
;;      (otherwise (error (format nil "Invalid encoder state: ~4,'0b" state))))))

;; (defparameter get-direction
;;   (let ((last-value nil))
;;     (lambda ()
;;       (let ((current-value (format nil "~a~a"
;; 				    (gpio-read *lpin* 'value)
;; 				    (gpio-read *hpin* 'value))))
;; 	(if (null last-value)
;; 	    (progn
;; 	      (setf last-value current-value)
;; 	      'neutral)
;; 	    (encoder-to-state last-value current-value))))))

(setf get-direction
      (lambda ()
	(sleep 1) 
	(nth (random 3) '(left right neutral))))

(defun process-direction (direction on-change)
  (when (not (eq direction 'neutral))
    (format t "Changed to ~a~%" direction)
    (funcall on-change direction)))

(defun start-rotary-encoder ()
  (setf *running* t)
  (loop
    (if *running*
	(process-direction (funcall get-direction) #'update-menu)
	(return))))

(defun stop-rotary-encoder ()
  (setf *running* nil))


;; all

(gpio-control *lpin* 'export)
(gpio-control *hpin* 'export)


(start-rotary-encoder)

(stop-rotary-encoder)



