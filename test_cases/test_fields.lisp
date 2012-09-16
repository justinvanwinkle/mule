;; (setf x (let ((_h (make-hash-table :test 'equal)))
;;   (setf
;;      (gethash "test" _h) "testval"
;;      (gethash "other" _h) "otherval")
;;   _h))
