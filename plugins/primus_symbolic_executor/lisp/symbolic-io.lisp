(declare (static opened-descriptors))
(declare (context (component bap:symbolic-computer)))

(defparameter *symbolic-io-bufer-size* 4096)
(defparameter *symbolic-stdin-size* 32768)
(defparameter *symbolic-initial-file-size* 16)

(defun symbolic-open (name flags)
  (declare (external "open"))
  (puts name)
  (incr opened-descriptors)
  (let ((name (symbol-of-string name))
        (desc (symbolic-value (symbol-concat 'open-desc- name))))
    (symbolic-assume (or (= desc -1)
                         (= desc opened-descriptors)))
    (if (= desc -1) -1
      (let ((size (symbolic-value  (symbol-concat 'open-size name)
                                   (word-width)
                                   *symbolic-initial-file-size*))
            (data (symbolic-memory (symbol-concat name)
                                   0 (-1 size))))
        (dict-add 'symbolic-open-size desc size)
        (dict-add 'symbolic-open-data desc data)
        (dict-add 'symbolic-open-fpos desc 0)
        desc))))


(defun symbolic-copy-to-concrete (memory cptr sptr len)
  (while (> len 0)
    (memory-write (+ cptr len -1)
                  (symbolic-memory-read memory (+ sptr len -1)))
    (decr len)))

(defun symbolic-copy-from-concrete (memory sdst csrc len)
  (while (> len 0)
    (symbolic-memory-write memory (+ sdst len -1)
                           (memory-read (+ csrc len -1)))
    (decr len)))


(defun read (fd buf len)
  (declare (external "read"))
  (if (= fd -1) -1
    (let ((fpos (dict-get 'symbolic-open-fpos fd))
          (size (dict-get 'symbolic-open-size fd))
          (data (dict-get 'symbolic-open-data fd))
          (len (min len (- size fpos) *symbolic-io-bufer-size*)))
      (symbolic-copy-to-concrete data buf fpos len)
      (dict-add 'symbolic-open-fpos fd (+ fpos len))
      len)))

(defun init-symbolic-stdin ()
  (dict-add 'symbolic-open-fpos 0 0)
  (dict-add 'symbolic-open-size 0 *symbolic-initial-file-size*)
  (dict-add 'symbolic-open-data 0
            (symbolic-memory 'stdin 0 (-1 *symbolic-initial-file-size*)))
  (set opened-descriptors 2))


(defmethod init ()
  (init-symbolic-stdin))
