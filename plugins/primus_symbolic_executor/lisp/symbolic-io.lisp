(declare (static opened-descriptors
                 opened-streams))
(declare (context (component bap:symbolic-computer)))

(defparameter *symbolic-io-bufer-size* 4096)
(defparameter *symbolic-stdin-size* 0x100)
(defparameter *symbolic-initial-file-size* 0x100)
(defconstant minimum-stream-number 0x8000)

(defun symbolic-open-input (name)
  (let ((next-desc (+1 opened-descriptors))
        (desc (symbolic-value (symbol-concat name '-desc) next-desc)))
    (symbolic-assume (or (= desc -1)
                         (= desc next-desc)))
    (if (= desc -1) -1
      (set opened-descriptors next-desc)
      (let ((size (symbolic-value  (symbol-concat name '-size)
                                   (word-width)
                                   *symbolic-initial-file-size*))
            (data (symbolic-memory (symbol-concat name)
                                   0 (-1 size))))
        (dict-add 'symbolic-open-size desc size)
        (dict-add 'symbolic-open-data desc data)
        (dict-add 'symbolic-open-fpos desc 0)
        desc))))


(defun open (name flags)
  (declare (external "open"))
  (symbolic-open-input (symbol-of-string name)))


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

(defun fopen (path mode)
  (declare (external "fopen"))
  (let ((name (symbol-of-string path))
        (desc (symbolic-open-input name)))
    (if (= desc -1) 0
      (incr opened-streams)
      (dict-add 'symbolic-streams opened-streams desc)
      opened-streams)))

(defun desc-of-stream (stream)
  (if (= stream 0) -1
    (or (dict-get 'symbolic-streams stream) 0)))

(defun fgetc (file)
  (declare (external "fgetc" "getc"))
  (let ((fd (desc-of-stream file)))
    (if (= fd -1) -1
    (let ((fpos (dict-get 'symbolic-open-fpos fd))
          (size (dict-get 'symbolic-open-size fd))
          (data (dict-get 'symbolic-open-data fd)))
      (if (= fpos size) -1
        (msg "fgetc($0) -> fpos($1) < size($2)" fd fpos size)
        (dict-add 'symbolic-open-fpos fd (+1 fpos))
        (symbolic-memory-read data fpos))))))

(defun init-symbolic-stdin ()
  (dict-add 'symbolic-open-fpos 0 0)
  (dict-add 'symbolic-open-size 0 *symbolic-initial-file-size*)
  (dict-add 'symbolic-open-data 0
            (symbolic-memory 'stdin 0 (-1 *symbolic-initial-file-size*)))
  (set opened-descriptors 2))


(defmethod init ()
  (init-symbolic-stdin)
  (set opened-streams minimum-stream-number))
