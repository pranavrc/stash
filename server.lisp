(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defvar *server* nil)

(defun initialize (host port)
  (let ((socket (usocket:socket-listen host port :reuse-address t)))
    (setf *server*
	  (bt:make-thread #'(lambda ()
			      (unwind-protect
				   (serve socket)
				(usocket:socket-close socket)))
			  :name (format nil "Server at ~a port" port)))))

(defun serve (socket)
  (loop
     (usocket:wait-for-input socket :timeout 5)
     (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
       (bt:make-thread #'(lambda ()
			   (with-open-stream (stream stream)
			     (handle-request stream)))
		       :name (format nil "Request handler for ~s" stream)))))

(defun handle-request (stream)
  (let ((line (read-line stream)))
    (format stream "~a" line))
  (terpri stream)
  (force-output stream))

#+bordeaux-threads
(defun terminate ()
  (let ((server (shiftf *server* nil)))
    (when server
      (bt:destroy-thread server))))
