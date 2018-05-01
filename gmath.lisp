;;;; math functions for vectors and lighting.

(defun normal (polygons index)
  "Returns the normal of the surface defined by POLYGONS at INDEX."
  (let ((temp1 (make-array 3))
        (temp2 (make-array 3)))
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (cross-product temp1 temp2)))

(defun cross-product (v1 v2)
  "Returns the cross-product of V1 and V2."
  (make-array 3 :initial-contents
              (list (- (* (svref v1 1)
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
                          (svref v2 0))))))
