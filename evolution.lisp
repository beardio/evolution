
(defparameter *width* 100)   ;;world
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))  ;;location of jungle in map
(defparameter *plant-energy* 80)   ;;80 days worth of food eating plant

;;save plants coordinates in a hash table, coords are the key
(defparameter *plants* (make-hash-table :test #'equal))


;;creates a new plant within specified region
(defun random-plant (left top width height)
  ;;pos gets (x . y)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    ;;set true in hash table for coord
    (setf (gethash pos *plants*) t)))

;;run everyday, one in jungle, one in world
(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))


(defstruct animal x y energy dir genes)

;;;;dir --->  |0|1|2
;;;;          |7|a|3
;;;;          |6|5|4

;;adam or eve
(defparameter *animals*
  (list (make-animal :x         (ash *width*  -1)
                     :y         (ash *height* -1)
                     :energy    1000
                     :dir       0
                     :genes     (loop repeat 8
                                   collecting (1+ (random 10))))
        (make-animal :x         (ash *width* -1)
                     :y         (ash *height* -1)
                     :energy    1000
                     :dir       0
                     :genes     (loop repeat 8
                                   collecting (1+ (random 10))))))  ;;;genes determine the animal movement (1 1 1 5 1 1) will face right
                                                                    ;;most of the time

;;;moves the animal, wraps animal in bounds 
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    ;;facing east add 1
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ;;facing add nothin
                                          ((or (= dir 1) (= dir 5)) 0)
                                          ;;facing west -1
                                          (t -1))
                                    *width*)
                                 *width*))
          ;;stays in bound
          (setf (animal-y animal) (mod (+ y
                                          (cond ((and (>= dir 0) (< dir 3)) -1)
                                                ((and (>= dir 4) (< dir 7)) 1)
                                                (t 0))
                                          *height*)
                                       *height*))
          ;;decrease animal health
          (decf (animal-energy animal))))


;;uses animal genes to decide how much will it turn
(defun turn (animal)
  ;;sums animal genes, and chooses random num in that sum
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;;traverse genes and finds x
    (labels ((angle (genes x)
               ;;subtracts x from the 1st 
               (let ((xnu (- x (car genes))))
                 ;;reached chosen num
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      ;;adds amount of turning to direction
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))

;;checks if plant at location, then eat
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
     (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


;;min energy level to reproduce
(defparameter *reproduction-energy* 200)

;;main func, lambda will call this func with animal from list
(defun reproduce (animald)
  ;; get current pos of the animal passed in
  ;;  (let ((pos (cons (animal-x animald) (animal-y animald)))))
  (let* ((animal-nu nil)
         (good nil)))
  
  (mapc (lambda (animal)
          ;;    (let ((pos-mate (cons (animal-x animal) (animal-y animal))))) 
          ;;(unless (eq animal-genes animald animal-genes animal)
          (if (= (animal-x animald) (animal-x animal))
              (if (= (animal-y animald) (animal-y animal))
                  (if (>= (animal-energy animald) *reproduction-energy*)
                      (if (>= (animal-energy animal) *reproduction-energy*)
                          (if (not (eql (animal-genes animal) (animal-genes animald)))
                              (progn
                                (setf (animal-energy animald) (ash (animal-energy animald) -1))
                                (setf (animal-energy animal) (ash (animal-energy animal) -1))
                                ;;(setf animal-nu (copy-structure animald)) 
                                ;;(setf mutation  (random 8))
                                (setf good t))))))))
        *animals*)
  (if good
      (let*  ((animal-nu (make-animal :x         (random 99)
                                      :y         (random 29)
                                      :energy    1000
                                      :dir       (random 8)
                                      :genes     (loop repeat 8
                                                    collecting (1+ (random 10))))))
        (push animal-nu *animals*))
      )
  )

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  ;;map thru all the animals
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))



(defun draw-world ()
  (loop for y
     below *height*
     do (progn (fresh-line)
               (princ "|")
               (loop for x
                  below *width*
                    ;;at least one is true
                  do (princ (cond ((some (lambda (animal)
                                           (and (= (animal-x animal) x)
                                                (= (animal-y animal) y)))
                                         *animals*)
                                   ;;if animal mark with M
                                   #\M)
                                  ;;check for plant, mark with *
                                  ((gethash (cons x y) *plants*) #\*)
                                  ;;otherwise just the space char
                                  (t #\space))))
               (princ "|"))))


(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                      below x
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ #\.))
                   (update-world))
               (evolution))))))
