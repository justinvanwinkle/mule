;;;; Parse and route generated HTTP requests in process, without socket overhead.

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(merge-pathnames "perfutils/case.lisp"
                           (or *compile-file-truename* *load-truename*))
        :verbose nil :print nil))

(defparameter *name* "http_routing")
(defparameter *description* "HTTP request parsing, query extraction, and handler dispatch")
(defparameter *default-size* 50000)
(defparameter *default-seed* 42)

(defparameter *crlf* (coerce (list #\Return #\Linefeed) 'simple-string))
(defparameter *crlf-crlf* (coerce (list #\Return #\Linefeed #\Return #\Linefeed) 'simple-string))

(defun string-partition (text sep)
  (declare (type simple-string text sep))
  (let ((pos (search sep text)))
    (if pos
        (values (subseq text 0 pos)
                sep
                (subseq text (+ pos (length sep))))
        (values text "" ""))))

(defun string-partition-char (text ch)
  (declare (type simple-string text)
           (type character ch))
  (let ((pos (position ch text)))
    (if pos
        (values (subseq text 0 pos)
                (string ch)
                (subseq text (+ pos 1)))
        (values text "" ""))))

(defun split-by-string (text sep)
  (declare (type simple-string text sep))
  (let ((sep-len (length sep))
        (start 0)
        (parts nil))
    (declare (type fixnum sep-len start))
    (loop
      (let ((pos (search sep text :start2 start)))
        (if pos
            (progn
              (push (subseq text start pos) parts)
              (setf start (+ pos sep-len)))
            (progn
              (push (subseq text start) parts)
              (return (nreverse parts))))))))

(defun split-by-char (text ch)
  (declare (type simple-string text)
           (type character ch))
  (let ((start 0)
        (parts nil))
    (declare (type fixnum start))
    (loop
      (let ((pos (position ch text :start start)))
        (if pos
            (progn
              (push (subseq text start pos) parts)
              (setf start (+ pos 1)))
            (progn
              (push (subseq text start) parts)
              (return (nreverse parts))))))))

(defun prepare (size seed)
  (declare (type fixnum size seed))
  (when (< size 0)
    (error "size must be nonnegative"))
  (let ((requests (make-array size))
        (state (logand seed #xFFFFFFFF)))
    (declare (type simple-vector requests)
             (type (unsigned-byte 32) state))
    (dotimes (index size requests)
      (declare (type fixnum index))
      (setf state (logand (+ (* state 1664525) 1013904223) #xFFFFFFFF))
      (let* ((selection (mod (ash state -16) 6))
             (identifier (write-to-string (mod state 100000)))
             (body "")
             (method "")
             (target ""))
        (cond
          ((= selection 0)
           (setf method "GET"
                 target "/health"))
          ((= selection 1)
           (setf method "GET"
                 target (concatenate 'string "/items?id=" identifier "&view=compact")))
          ((or (= selection 2) (= selection 3))
           (setf method "POST"
                 target "/events"
                 body (concatenate 'string
                                   "event=" identifier
                                   "&source=worker" (write-to-string (mod index 8)))))
          ((= selection 4)
           (setf method "GET"
                 target (concatenate 'string "/missing/" identifier)))
          (t
           (setf method "DELETE"
                 target (concatenate 'string "/items?id=" identifier))))
        (let ((content-length (+ (length body) (if (= selection 3) 1 0))))
          (setf (svref requests index)
                (concatenate 'string
                             method " " target
                             " HTTP/1.1" *crlf*
                             "Host: localhost" *crlf*
                             "X-Request-ID: " identifier *crlf*
                             "Content-Length: " (write-to-string content-length) *crlf*
                             "Connection: keep-alive" *crlf-crlf*
                             body)))))))

(defun handle (request)
  (declare (type simple-string request))
  (multiple-value-bind (head sep body) (string-partition request *crlf-crlf*)
    (declare (ignore sep)
             (type simple-string head body))
    (let* ((lines (split-by-string head *crlf*))
           (request-line (the simple-string (first lines)))
           (request-parts (split-by-char request-line #\Space))
           (method (the simple-string (first request-parts)))
           (target (the simple-string (second request-parts))))
      (multiple-value-bind (path qsep query) (string-partition-char target #\?)
        (declare (ignore qsep)
                 (type simple-string path query))
        (let ((content-length 0))
          (declare (type fixnum content-length))
          (dolist (line (rest lines))
            (multiple-value-bind (name csep value)
                (string-partition-char (the simple-string line) #\:)
              (declare (ignore csep)
                       (type simple-string name value))
              (when (string= (string-downcase name) "content-length")
                (setf content-length
                      (parse-integer (string-trim '(#\Space #\Tab #\Return #\Linefeed) value))))))
          (when (and (string= method "GET") (string= path "/health"))
            (return-from handle (values 1 200 (format nil "ok~%"))))
          (when (string= path "/items")
            (unless (string= method "GET")
              (return-from handle (values 2 405 (format nil "method not allowed~%"))))
            (let ((identifier ""))
              (dolist (field (split-by-char query #\&))
                (multiple-value-bind (name esep value)
                    (string-partition-char (the simple-string field) #\=)
                  (declare (ignore esep))
                  (when (string= name "id")
                    (setf identifier value))))
              (return-from handle
                (values 2 200 (concatenate 'string "{\"id\":" identifier "}" (string #\Linefeed))))))
          (when (and (string= method "POST") (string= path "/events"))
            (if (/= content-length (length body))
                (return-from handle (values 3 400 (format nil "invalid content length~%")))
                (return-from handle
                  (values 3 202
                          (concatenate 'string
                                       "{\"accepted\":"
                                       (write-to-string content-length)
                                       "}"
                                       (string #\Linefeed))))))
          (values 4 404 (format nil "not found~%")))))))

(defun run-kernel (data)
  (declare (type simple-vector data))
  (let ((routes 0)
        (statuses 0)
        (response-bytes 0)
        (n (length data)))
    (declare (type fixnum routes statuses response-bytes n))
    (dotimes (i n)
      (declare (type fixnum i))
      (multiple-value-bind (route status response)
          (handle (the simple-string (svref data i)))
        (declare (type fixnum route status)
                 (type simple-string response))
        (incf routes route)
        (incf statuses status)
        (incf response-bytes (length response))))
    (list routes statuses response-bytes)))

(defun main ()
  (run-case *name* *description* *default-size* *default-seed* #'prepare #'run-kernel))

(eval-when (:execute)
  (main))
