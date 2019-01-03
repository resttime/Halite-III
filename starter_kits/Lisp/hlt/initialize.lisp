(in-package :halite)

;; Local

(defun json-key (pair)
  "Get the key from json pair string in the format '\"KEY\":VAL'"
  (let ((start (1+ (position #\" pair)))
        (end (position #\" pair :from-end t)))
    (read-from-string (substitute #\- #\_ (subseq pair start end)))))

(defun json-val (pair)
  "Get the value from a json pair string in the format '\"KEY\":VAL'"
  (let ((val (subseq pair (1+ (position #\: pair :from-end t)))))
    (cond ((string= val "true") t)
          ((string= val "false") nil)
          (t (read-from-string val)))))

;; Export

(defun parse-json (json)
  "Parse the json of constants per pair to an idiomatic lisp alist"
  (loop for start = (position #\" json) then (position #\" json :start finish)
        while start
        for finish = (position-if #'(lambda (c) (or (char= c #\,) (char= c #\})))
                                  json :start start)
        while finish
        for pair = (subseq json start finish)
        collect (list (json-key pair) (json-val pair))))

(defun read-line-integer (&rest args)
  "Hacky way to read a string of integers as a list"
  (read-from-string (concatenate 'string "(" (apply #'read-line args) ")")))
