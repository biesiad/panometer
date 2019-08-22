
(defun start (index range min)
  (max min (- index range)))

(defun end (index range max)
  ;; (min max (+ index range 1)))
  (+ 1 index))

(defun smooth (input range)
  (labels ((iter (index output)
	     (cond ((= index (length input)) (reverse output))
		   (t
		    (iter
			(1+ index)
			(cons
			 (/ (apply #'+ (subseq input
					       (start index range 0)
					       (end index range (length input))))
			    (float (- (end index range (length input))
				      (start index range 0))))
			 output))))))
    (iter 0 '())))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for sample = (read-line in nil)
	  while sample
	  collect (parse-integer sample))))

(defun save (input filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (dolist (sample (smooth input (* 60 60)))
      (write-line (write-to-string sample) out))))

(save
 (smooth (read-input "./panometer_clean2.data") (* 60 60))
 "panometer2_smooth_3600_oneway.csv")

(save
 (smooth (read-input "./panometer_clean3.data") (* 60 60))
 "panometer3_smooth_3600_oneway.csv")

(save
 (smooth (read-input "./panometer_clean4.data") (* 60 60))
 "panometer4_smooth_3600_oneway.csv")

(smooth '(0 1 0) 1)
(smooth '(0 1 2 1 0) 1)
(smooth '(1 1 1 1 1) 1)

(smooth '(0 1 0) 3)
(smooth '(0 1 2 1 0) 5)

(smooth '(0 1 2 3 4 5 6 7 8 9) 1)
(smooth '(1 3 2 4 4 5 4 6 7 5 6 5 3 2 1))



(defmacro addf (to value)
  `(setf ,to (+ ,to ,value)))

(defmacro running-addf (to slot sample samples)
  `(addf ,to (+ (slot-value ,sample ,slot)
	       (- (slot-value (first (reverse ,samples)) ,slot)))))

(defun make-smooth (count)
  (let ((samples '())
	(sum-temperature 0)
	(sum-humidity 0)
	(sum-height 0)
	(sum-luminosity 0))
    (lambda (sample)
      (push sample samples)
      (cond ((<= (length samples) count)
	     (addf sum-temperature (sample-temperature sample))
	     (addf sum-humidity (sample-humidity sample))
	     (addf sum-height (sample-height sample))
	     (addf sum-luminosity (sample-luminosity sample)))
	     (t
	      (running-addf sum-temperature 'temperature sample samples)
	      (running-addf sum-humidity 'humidity sample samples)
	      (running-addf sum-height 'height sample samples)
	      (running-addf sum-luminosity 'luminosity sample samples)
	      (setf samples (reverse (rest (reverse samples))))))
      (make-sample
       :experiment (sample-experiment sample)
       :temperature (/ sum-temperature (length samples))
       :humidity (/ sum-humidity (length samples))
       :height (/ sum-height (length samples))
       :luminosity (/ sum-luminosity (length samples))))))
