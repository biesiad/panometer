(defpackage :panometer-core
  (:use :cl)
  (:export :start-experiment
	   :stop-experiment
	   :delete-experiment
	   :*running*
	   :get-experiments))

(in-package :panometer-core)


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

(defun display (experiment)
  (uiop:run-program
   (format nil "python display.py ./assets/~d_downsample.ppm" experiment)))


(defun downsample (samples count)
  (if (> count (length samples))
      samples
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
	  (iter samples '() 0 ratio ratio)))))

(defun add-sample (sample experiment)
  (ensure-directories-exist (experiment-file experiment))
  (let ((out (open (experiment-file experiment)
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)))
    (format-sample sample out)
    (close out)))

(defun ppm (experiment columns)
  (let* ((data (make-array `(64 ,columns) :initial-element 0))
	 (samples (mapcar
		   #'sample-height
		   (downsample (get-samples experiment) columns)))
	 (offset-neg (max (- (apply #'min samples) 10) 0))
	 ;; using last to smooth the curve by calculating a mean of last 5 samples
	 (last '()))
    (loop for column from 0
       and height in samples
       do
	 (if (> 5 (length last))
	     (push height last)
	     (setf last (reverse (cdr (reverse last)))))
	 (setf (aref data
		     (min 63
			  (floor (- (max (/ (apply #'+ last)
					    (length last))
					 0)
				    offset-neg)))
		     column)
	       1))
    data))

(defun format-ppm (data columns stream)
  (format stream "P1~%")
  (format stream "~d 64~%" columns)
  (loop for row from 63 downto 0 do
    (loop for column from 0 below columns do
	 (format stream "~a " (aref data row column)))
    (format stream "~%")))

(defun format-ppm-binary (data columns stream)
  (with-input-from-string (is (format nil "P6~%~d 64~%255~%" columns))
    (do ((c (read-char is) (read-char is nil 'the-end)))
	((not (characterp c)))
      (write-byte (char-code c) stream)))
  
  (loop for row from 63 downto 0 do
    (loop for column from 0 below columns do
	 (if (= (aref data row column) 1)
	     (progn
	       (write-byte 255 stream)
	       (write-byte 255 stream)
	       (write-byte 255 stream))
	     (progn
	       (write-byte 0 stream)
	       (write-byte 0 stream)
	       (write-byte 0 stream))))))


(defun process-sample (sample experiment)
  (format-sample sample)
  (add-sample sample experiment)

  (plot experiment)

  ;; downsample and save experiment data
  (save-samples (downsample (get-samples experiment) *downsample*)
		experiment
		"_downsample")
  ;; (plot experiment "_downsample")

  ;; render downsampled ppm and display
  (with-open-file (stream
		   (format nil "assets/~d_downsample.ppm" experiment)
		   :element-type '(unsigned-byte 8)
		   :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (format-ppm-binary (ppm experiment *downsample*)
	      *downsample*
	      stream))
    (display experiment))

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
     (sleep 60))
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
    	(plot-downsample (format nil "./assets/~d_downsample.png" experiment))
	(ppm (format nil "./assets/~d_downsample.ppm" experiment)))
    (when (probe-file data) (delete-file data))
    (when (probe-file data-downsample) (delete-file data-downsample))
    (when (probe-file plot) (delete-file plot))
    (when (probe-file plot-downsample) (delete-file plot-downsample))
    (when (probe-file ppm) (delete-file ppm))))


