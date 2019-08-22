(defpackage :panometer-web
  (:use :cl :panometer))

(in-package :panometer/web)

(require :cl-json)
(require :hunchentoot)
(require :easy-routes)
(require :cl-who)
	 
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

;; (hunchentoot:start
;;  (make-instance 'easy-routes:routes-acceptor
;; 		:name :panometer
;;  		:port *port*))

;; (format t "Started server on ~a" *port*)


