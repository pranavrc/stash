(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defvar *server* nil)

(defun initialize (port &optional (host usocket:*wildcard-host*))
  "Initialize the server at the host's port"
  (let ((socket (usocket:socket-listen host port :reuse-address t)))
    (setf *server*
	  (bt:make-thread #'(lambda ()
			      (unwind-protect
				   (serve socket)
				(usocket:socket-close socket)))
			  :name (format nil "Server at ~a port" port)))))

(defun serve (socket)
  "Create a socket stream and open it"
  (loop
     (usocket:wait-for-input socket :timeout 5)
     (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
       (bt:make-thread #'(lambda ()
			   (with-open-stream (stream stream)
			     (handle-request stream)))
		       :name (format nil "Request handler for ~s" stream)))))

(defun handle-request (stream)
  "Handle stream data"
  (let ((line (read-line stream)))
    (format stream "~a" line))
  (terpri stream)
  (force-output stream))

#+bordeaux-threads
(defun terminate ()
  "Terminate the server thread"
  (let ((server (shiftf *server* nil)))
    (when server
      (bt:destroy-thread server))))

(defun client (port command &optional (host usocket:*wildcard-host*))
  "Client function to write to the server stream"
  (let* ((socket (usocket:socket-connect host port))
	 (stream (usocket:socket-stream socket)))
    (write-line command stream)
    (force-output stream)
    (let ((result (read-line stream)))
      (close stream)
      (usocket:socket-close socket)
      result)))

(defun string-split (string delim)
  "Splits a string into substrings around the delimiter."
  (loop for x = 0 then (1+ y)
     as y = (position delim string :start x)
     collect (subseq string x y)
     while y))
