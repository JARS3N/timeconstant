(defun tau (period delta-p delta-t) 
 "calculating time constant"
 (abs(/ period (log (/ 1 (- 1 (/ delta-p delta-t)))))))
 
 
