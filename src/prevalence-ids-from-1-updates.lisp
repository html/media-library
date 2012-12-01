(in-package :weblocks-prevalence)

(defmethod initialize-instance :after ((obj persistent-objects-of-class) &rest args) 
  (when (= (persistent-objects-of-class-next-id obj) -1)
    (setf (persistent-objects-of-class-next-id obj) 0)))
