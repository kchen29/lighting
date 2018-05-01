;;;; math functions for vectors and lighting.

;;closure, for fun
(let ((temp1 (make-array 3))
      (temp2 (make-array 3)))
  (defun normal (polygons index)
    "Returns the normal of the surface defined by POLYGONS at INDEX."
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (cross-product temp1 temp2)))

(defun cross-product (v1 v2)
  "Returns the cross-product of V1 and V2."
  (vector (- (* (svref v1 1)
                (svref v2 2))
             (* (svref v1 2)
                (svref v2 1)))
          (- (* (svref v1 2)
                (svref v2 0))
             (* (svref v1 0)
                (svref v2 2)))
          (- (* (svref v1 0)
                (svref v2 1))
             (* (svref v1 1)
                (svref v2 0)))))

;;define as dynamic variables for now
;;closure later
;;everything are lists for now
(defparameter ambient '(50 50 50))
(defparameter view '(0 0 1))
(defparameter light '(1 1 1))
(defparameter light-color '(255 0 255))
(defparameter areflect '(0.1 0.1 0.1))
(defparameter dreflect '(0.5 0.5 0.5))
(defparameter sreflect '(0.5 0.5 0.5))

(defun magnitude (vector)
  "Calculates the magnitude of the vector."
  (sqrt (loop for x in vector
              sum (* x x))))

(defun normalize (vector)
  "Normalizes the vector."
  (loop for x in vector
        with magnitude = (magnitude vector)
        collect (/ x magnitude)))

(defun hadamard-* (v1 v2)
  "Multiples V1 and V2 component-wise. The Hadamard product."
  (mapcar #'* v1 v2))

(defun +-v (&rest vectors)
  "Adds VECTORS together."
  (apply #'mapcar #'+ vectors))

(defun scale-v (vector scalar)
  "Scales VECTOR by SCALAR."
  (mapcar (lambda (x) (* x scalar)) vector))

(defun dot (v1 v2)
  "Dots V1 and V2."
  (loop for x in v1
        for y in v2
        sum (* x y)))

(defun bound (color)
  "Checks and fixes the values of color."
  (mapcar (lambda (x) (min 255 (max 0 x))) color))

(defun rbound (color)
  "Rounds and checks bounds of color."
  (bound (mapcar #'round color)))

(defun calculate-color (normal)
  "Calculates the color given the NORMAL surface."
  (let ((normal (normalize normal))
        (light (normalize light)))
    (rbound
     (+-v (bound
           (hadamard-* ambient areflect))
          (bound
           (scale-v (hadamard-* light-color dreflect)
                    (dot normal light)))
          (bound
           (scale-v (hadamard-* light-color sreflect)
                    (dot view (+-v (scale-v normal
                                            (* 2 (dot normal light)))
                                   (scale-v light -1)))))))))
