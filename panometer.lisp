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

(defvar *container-height* 60)

(defvar *running* nil)

(defvar *paused* nil)

(defvar *downsample* 128)


(defstruct sample
  time
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

(defun parse-raw-sample (input)
  (or (ignore-errors
	(let ((values (split-by-comma input)))
	  (make-sample
	   :time (get-universal-time)
	   :temperature (parse-float (nth 0 values))
	   :humidity (parse-float (nth 1 values))
	   :height (- *container-height* (parse-float (nth 2 values)))
	   :luminosity (parse-float (nth 3 values)))))
      (make-sample
       :time (get-universal-time)
       :temperature 0.0
       :humidity 0.0
       :height 0.0
       :luminosity 0.0)))

(defun parse-sample (input)
  (let ((parts (split-by-comma input)))
    (make-sample
     :time (parse-integer (nth 0 parts))
     :temperature (parse-float (nth 1 parts))
     :humidity (parse-float (nth 2 parts))
     :height (parse-float (nth 3 parts))
     :luminosity (parse-float (nth 4 parts)))))

(defun format-sample (sample &optional (stream *standard-output*))
  (format stream "~a,~a,~a,~a,~a~%"
	  (sample-time sample)
	  (sample-temperature sample)
	  (sample-humidity sample)
	  (sample-height sample)
	  (sample-luminosity sample)))


(defun experiment-file (experiment &optional (suffix ""))
  (concatenate 'string
	       "./experiments/"
	       (write-to-string experiment)
	       suffix
	       ".csv"))

(defun plot-file (experiment &optional (suffix ""))
  (format nil "./assets/~d~a.png"
	  (write-to-string experiment)
	  suffix))


(defun get-experiments ()
  (labels ((path-to-experiment (path)
	     (read-from-string
	      (subseq
	       (file-namestring path)
	       0
	       (position #\. (file-namestring path))))))

    (reverse (mapcar #'path-to-experiment
		     (remove-if (lambda (path) (position #\_ (file-namestring path)))
				(uiop:directory-files "./experiments/"))))))

(defun get-samples (experiment)
  (with-open-file (file (experiment-file experiment))
    (loop for line = (read-line file nil nil)
	  while line
	  collect (parse-sample line))))


(defun save-samples (samples experiment &optional (suffix ""))
  (let ((out (open (format nil "experiments/~d~d.csv" experiment suffix)
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :overwrite)))
    (dolist (sample samples)
     (format-sample sample out))
    (close out)))

(defun plot (experiment &optional (suffix ""))
  (ensure-directories-exist (plot-file experiment suffix))
  (uiop:run-program
   (format nil "gnuplot -c plot.plt ~d ~d"
	   (experiment-file experiment suffix)
	   (plot-file experiment suffix))))

(defun downsample (samples count)
  (labels ((iter (samples result index ratio next)
	     (cond ((null samples) (reverse result))
		   ((=
		     (floor (+ index ratio))
		     (floor next))
		    (iter (rest samples)
			  (cons (first samples) result)
			  (+ index 1)
			  ratio
			  (+ next ratio)))
		   (t
		    (iter (rest samples)
			  result
			  (+ index 1)
			  ratio
			  next)))))

    (let ((ratio (/ (length samples) count)))
      (iter samples '() 0 ratio ratio))))

(defun add-sample (sample experiment)
  (ensure-directories-exist (experiment-file experiment))
  (let ((out (open (experiment-file experiment)
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)))
    (format-sample sample out)
    (close out)))

(defparameter samples (mapcar
		 #'sample-height
		 (downsample (get-samples (nth 10 (get-experiments))) 10)))

(defun ppm (experiment columns)
  (let* ((data (make-array `(64 ,columns) :initial-element 0))
	 (samples (mapcar
		   #'sample-height
		   (downsample (get-samples experiment) columns)))
	 (offset (apply #'min samples)))
    (loop for index from 0
       and height in samples
       do
	 (when (> height 0)
	   (setf (aref data (floor (+ 10 (- height offset))) index) 1)))
    data))

(defun format-ppm (data columns stream)
  (format stream "P1~%")
  (format stream "~d 64~%" columns)
  (loop for row from 0 below 64 do
    (loop for column from 0 below columns do
      (format stream "~a " (aref data row column)))
    (format stream "~%")))


(defun process-sample (sample experiment)
  (format-sample sample)
  (add-sample sample experiment)
  (plot experiment)

  (save-samples (downsample (get-samples experiment) *downsample*)
		experiment
		"_downsample")
  (plot experiment "_downsample")

  (with-open-file (stream
		   (format nil "assets/~d_downsample.ppm" experiment)
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (format-ppm (ppm experiment *downsample*)
	      *downsample*
	      stream)))


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
	    (process-sample (parse-raw-sample (get-sample)) experiment)))
     (sleep 10))
  (format t "Stopping~%"))

(defun stop-experiment ()
  (setf *running* nil)
  (setf *paused* nil))

(defun pause-experiment ()
  (setf *paused* t))

(defun resume-experiment ()
  (setf *paused* nil))

(defun delete-experiment (experiment)
  (when (equal experiment *running*)
    (stop-experiment))
  (let ((data (format nil "./experiments/~d.csv" experiment))
	(data-downsample (format nil "./experiments/~d_downsample.csv" experiment))
	(plot (format nil "./assets/~d.png" experiment))
    	(plot-downsample (format nil "./assets/~d_downsample.png" experiment)))
    (when (probe-file data) (delete-file data))
    (when (probe-file data-downsample) (delete-file data-downsample))
    (when (probe-file plot) (delete-file plot))
    (when (probe-file plot-downsample) (delete-file plot-downsample))))

			  
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

(easy-routes:defroute get-experiments-route
    ("/experiments") ()
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

(easy-routes:defroute get-experiment-route
    "/experiments/:id" ()
  (let ((image-url (format nil  "~a/~a.png" *hostname* id))
	(image-downsample-url (format nil  "~a/~a_downsample.png" *hostname* id))
	(fallback (format nil "this.src=\"~a/loader.gif\"" *hostname*))
	(styles (format nil "~a/styles.css" *hostname*))
	(name (experiment-to-time (read-from-string id)))
    	(delete-url (format nil "/experiments/~d/delete" (read-from-string id)))
	(data (format nil "./experiments/~d.csv" (read-from-string id)))
	(plot (format nil "./assets/~d.png" (read-from-string id))))
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:head
       (:link :rel "stylesheet" :type "text/css" :href styles)
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=yes")
       (:meta :http-equiv "refresh" :content "10"))
      (:body
       (:p :class "logo" (cl-who:str name))
       (:div :class "container"       
	     (when (and (probe-file data)
			(probe-file plot))
	       (cl-who:htm (:form :method :post
				  :action delete-url
				  (:input :type "submit"
					  :value "delete"
					  :class "link"))))
	     (:p (:img :src image-url :onerror fallback))
	     (:p (:img :src image-downsample-url :onerror fallback))
	     (when (equal *running* (read-from-string id))
	       (cl-who:htm (:form :method :post
				  :action "/experiments/stop"
				  (:input :type "submit" :value "stop" :class "button"))))
	     (:p (:a :class "button" :href "/experiments" "back")))))))

(easy-routes:defroute start-experiment-route
    ("/experiments" :method :post) ()
  (cond (*running*
	 (hunchentoot:redirect (experiment-to-url *running*)))
	(t 
	 (let ((experiment (get-universal-time)))
	   (process-run-function experiment #'start-experiment experiment)
	   (hunchentoot:redirect (experiment-to-url experiment))))))

(easy-routes:defroute stop-experiment-route
    ("/experiments/stop" :method :post) ()
  (stop-experiment)
  (hunchentoot:redirect "/experiments"))

(easy-routes:defroute delete-experiment-route
    ("/experiments/:id/delete" :method :post) ()
  (delete-experiment (read-from-string id))
  (hunchentoot:redirect "/experiments"))

;; (easy-routes:defroute pause-experiment-route
;;     ("/experiemnts/:id/pause" :method :post) ()
;;   (pause-experiment (read-from-string id))
;;   (hunchentoot:redirect (format nil "/experiments/~d" id)))

;; (easy-routes:defroute resume-experiment-route
;;     ("/experiemnts/:id/resume" :method :post) ()
;;   (resume-experiment (read-from-string id))
;;   (hunchentoot:redirect (format nil "/experiments/~d" id)))

(hunchentoot:start
 (make-instance 'easy-routes:routes-acceptor
		:name :panometer
 		:port *port*))

(format t "Started server on ~a" *port*)
