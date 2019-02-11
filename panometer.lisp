(require :cl-json)
(require :hunchentoot)
(require :easy-routes)
(require :cl-who)

;; (defpackage :panometer
;;   (:use
;;    :cl
;;    :hunchentoot
;;    :cl-who
;;    :easy-routes))

;; (in-package :panometer)

(defvar *container-height* 70)

(defvar *running* nil)

(defvar *paused* nil)

(defstruct sample
  experiment
  temperature
  humidity
  height
  luminosity)

(defun get-sample ()
  (uiop:run-program
   "echo \"$(python dht11.py),$(python3 vl6180.py)\""
   :output :string))

(defun split-by-comma (string)
  (loop for i = 0 then (1+ j)
	as j = (position #\, string :start i)
	collect (subseq string i j)
	while j))

(defun parse-float (string)
  (float (with-input-from-string (in string) (read in))))

(defun parse-sample (experiment input)
  (let ((values (split-by-comma input)))
    (make-sample
     :experiment experiment
     :temperature (parse-float (nth 0 values))
     :humidity (parse-float (nth 1 values))
     :height (- *container-height* (parse-float (nth 2 values)))
     :luminosity (parse-float (nth 3 values)))))

(defun format-sample (sample &optional (stream *standard-output*))
  (format stream "~a,~a,~a,~a,~a~%"
	  (get-universal-time)
	  (sample-temperature sample)
	  (sample-humidity sample)
	  (sample-height sample)
	  (sample-luminosity sample)))

(defun experiment-file (experiment)
  (concatenate 'string
	       "./experiments/"
	       (write-to-string experiment)
	       ".csv"))

(defun plot-file (experiment)
  (concatenate 'string
	       "./assets/"
	       (write-to-string experiment)
	       ".png"))

(defun plot (experiment)
  (ensure-directories-exist (plot-file experiment))
  (uiop:run-program
   (concatenate 'string
		"gnuplot -c plot.plt "
		(experiment-file experiment)
		" "
		(plot-file experiment))))

(defun save-sample (sample)
  (ensure-directories-exist (experiment-file (sample-experiment sample)))
  (let ((out (open (experiment-file (sample-experiment sample))
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)))
    (format-sample sample out)
    (close out)))

(defun process (sample)
  (format-sample sample)
  (save-sample sample)
  (plot (sample-experiment sample)))

(defun start-experiment (experiment)
  (setf *running* experiment)
  (setf *paused* nil)
  (format t "Starting ~a~%" experiment)
  (loop
     (cond ((not *running*)
	    (return))
	   (*paused*
	    (format t "Paused~%"))
	   (t
	    (process (parse-sample experiment (get-sample)))))
     (sleep 10))
  (format t "Stopping~%"))

(defun stop-experiment ()
  (setf *running* nil))

(defun pause-experiment ()
  (setf *paused* t))

(defun resume-experiment ()
  (setf *paused* nil))

(defun path-to-experiment (path)
  (read-from-string
   (subseq
    (file-namestring path)
    0
    (position #\. (file-namestring path)))))

(defun get-experiments ()
  (reverse (mapcar #'path-to-experiment
	  (uiop:directory-files "./experiments/"))))


;; web

(defparameter *hostname* "http://192.168.1.7")

(defparameter *port* 8888)

(defun experiment-to-url (experiment)
  (format nil
	  "~a:~a/experiments/~a"
	  *hostname*
	  *port*
	  experiment))

(defun experiment-link-classname (experiment)
  (if (equal experiment *running*)
      "experiment-link active"
      "experiment-link"))

(defun experiment-to-time (experiment)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (parse-integer (format nil "~d" experiment)))
    (declare (ignore second))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(easy-routes:defroute get-experiments-route ("/experiments") ()
  (let ((styles (format nil "~a/styles.css" *hostname*)))
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:head
       (:link :rel "stylesheet" :type "text/css" :href styles)
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=yes"))
      (:body
       (:p :class "logo" "PANOMETER")
       (:div :class "container"
	     (when (not *running*)
	       (cl-who:htm (:form :method :post :action
				  "/experiments"
				  (:input :class "button" :type "submit" :value "start"))))
	     (loop for e in (get-experiments) do
	       (let ((url (format nil "~a:~a/experiments/~a" *hostname* *port* e))
		     (classname (experiment-link-classname e))
		     (name (experiment-to-time e)))
		 (cl-who:htm
		  (:p (:a :class classname :href url (cl-who:str name)))))))))))

(intern "1232")

(easy-routes:defroute get-experiment-route "/experiments/:id" ()
  (let ((image-url (format nil  "~a/~a.png" *hostname* id))
	(fallback (format nil "this.src=\"~a/fallback.png\"" *hostname*))
	(styles (format nil "~a/styles.css" *hostname*))
	(name (experiment-to-time (read-from-string id))))
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:head
       (:link :rel "stylesheet" :type "text/css" :href styles)
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=yes")
       (:meta :http-equiv "refresh" :content "10"))
      (:body
       (:p :class "logo" (cl-who:str name))
       (:div :class "container"       
	     (:p (:img :src image-url :onerror fallback))
	     (when (equal *running* (read-from-string id))
	       (cl-who:htm (:form :method :post
				  :action "/experiments/stop"
				  (:input :type "submit" :value "stop" :class "button"))))
	     (:p (:a :class "button" :href "/experiments" "back")))))))

(easy-routes:defroute start-experiment-route ("/experiments" :method :post) ()
  (cond (*running*
	 (hunchentoot:redirect (experiment-to-url *running*)))
	(t 
	 (let ((experiment (get-universal-time)))
	   (process-run-function experiment #'start-experiment experiment)
	   (hunchentoot:redirect (experiment-to-url experiment))))))

(easy-routes:defroute stop-experiment-route ("/experiments/stop" :method :post) ()
  (stop-experiment)
  (hunchentoot:redirect "/experiments"))

(hunchentoot:start
 (make-instance 'easy-routes:routes-acceptor
 		:port 8888))
