(ql:quickload :usocket)
(ql:quickload :bordeaux-threads)

(defvar *server* nil)
(defparameter *store* (make-hash-table :test 'equal))

(defun serve (port &optional (host usocket:*wildcard-host*))
  "Serve at the host's port"
  (let ((socket (usocket:socket-listen host port :reuse-address t)))
    (setf *server*
	  (bt:make-thread #'(lambda ()
			      (unwind-protect
				   (serve socket)
				(usocket:socket-close socket)))
			  :name (format nil "Server at ~a port" port)))))

(defun initialize (socket)
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
    (format stream "~a" (parse line)))
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

(defun set-key-in-store (key value store)
  "Set key in hashtable to value"
  (let* ((exists (gethash key store)))
    (if exists
	"Key exists. Delete before re-inserting."
	(progn
	  (setf (gethash key store) value)
	  "Stored."))))

(defun get-value-from-store (key store)
  "Get value of key from hashtable"
  (let* ((value (gethash key store)))
    (if value
	(format nil "~a" value)
	(format nil "~a wasn't found." key))))

(defun remove-from-store (key store)
  "Remove key and value from hashtable"
  (let* ((value (remhash key store)))
    (if value
	(format nil "~a has been removed." key)
	(format nil "~a wasn't found." key))))

(defun string-split (string delim)
  "Splits a string into substrings around the delimiter."
  (loop for x = 0 then (1+ y)
     as y = (position delim string :start x)
     collect (subseq string x y)
     while y))

(defun parse (command)
  "Parse an input set or get command."
  (let* ((params (string-split command #\=))
	 (firstparam (strip-whitespaces (first params))))
    (if (> (list-length params) 1)
	(set-key-in-store firstparam (strip-whitespaces (first (last params))) *store*)
	(if (char= (char firstparam 0) #\!)
	    (remove-from-store (strip-whitespaces (subseq firstparam 1)) *store*)
	    (get-value-from-store firstparam *store*)))))

(defun hash-keys (hash-table)
  "Print all keys in hashtable."
  (loop for key being the hash-keys of hash-table collect key))

(defun strip-whitespaces (string)
  "Strip trailing and leading whitespaces from string."
  (string-trim " " string))
