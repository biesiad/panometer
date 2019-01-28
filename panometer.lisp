(defpackage :panometer
  (:use :cl))

(in-package :panometer)

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

(defun experiment-file (experiment &optional suffix)
  (concatenate 'string
	       "./experiments/"
	       (write-to-string experiment)
	       (or suffix "")
	       ".csv"))

(defun plot-file (experiment &optional suffix)
  (concatenate 'string
	       "./plots/"
	       (write-to-string experiment)
	       (or suffix "")
	       ".png"))

(defun plot (experiment)
  (ensure-directories-exist (plot-file experiment))
  (uiop:run-program
   (concatenate 'string
		"gnuplot -c plot.plt "
		(experiment-file experiment)
		" "
		(plot-file experiment))))

(defun save-sample (sample &optional suffix)
  (ensure-directories-exist (experiment-file (sample-experiment sample)))
  (let ((out (open (experiment-file (sample-experiment sample) suffix)
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
  ;; (when (probe-file (experiment-file experiment))
  ;;   (delete-file (experiment-file experiment)))
  (setf *running* t)
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

(start-experiment 3757606263)
;; (stop-experiment)
