(in-package #:cl-user)
(defpackage #:hbot
  (:use #:cl #:halite)
  (:export #:main))
(in-package #:hbot)

(defun init ()
  "Initalize the game constants"
  (let* ((constants (parse-json (read-line)))
         (player-meta (mapcar #'list '(num-players my-id) (read-line-integer)))
         (player-info (loop for p below (second (assoc 'num-players player-meta))
                            for line = (read-line-integer)
                            collect (mapcar #'list '(id x y) line)))
         (map-meta (mapcar #'list '(map-width map-height) (read-line-integer)))
         (map-info (make-array (list (second (assoc 'map-width map-meta))
                                     (second (assoc 'map-height map-meta)))
                               :initial-contents
                               (loop for p below (second (assoc 'map-height map-meta))
                                     for line = (read-line-integer)
                                     collect line))))
    (write-line (concatenate 'string "Bot"
                             (write-to-string (random 100 (make-random-state t)))))))

(defun update-turn ()
  (let ((turn (car (read-line-integer))))))

(defun main ()
  "Entry point for the compilation"
  (loop (print (read-line))))
