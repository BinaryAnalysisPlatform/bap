(require libc-init)
(require memory)
(require types)


(defun fputc (char stream)
  (declare (external "fputc" "putc"))
  (if (= 0 (channel-output stream char)) char -1))

(defun putchar (char)
  (declare (external "putchar"))
  (fputc char *standard-output*))

(defun fputs (p stream)
  (declare (external "fputs"))
  (while (not (points-to-null p))
    (fputc (cast int (memory-read p)) stream)
    (incr p))
  (fputc 0xA stream))

(defun puts (p)
  (declare (external "puts"))
  (fputs p *standard-output*))


;; the channel module have rough equality between streams and
;; file descriptors, as they both are represented as integers. We are currently
;; ignoring modes, we will add them later, of course.
(defun fopen (path mode)
  (declare (external "fopen" "open"))
  (channel-open path))

(defun open3 (path flags mode)
  (declare (external "open"))
  (fopen path mode))

(defun output-item-nth-char (ptr size item fd i)
  (= 0 (channel-output
        fd
        (memory-read (+ ptr (* size item) i)))))

(defun output-item (buf size item fd)
  (let ((i 0))
    (while (and
            (< i size)
            (output-item-nth-char buf size item fd i))
      (incr i))
    i))

(defun fwrite (buf size n stream)
  (declare (external "fwrite"))
  (let ((i 0))
    (while (and (< i n)
                (= size (output-item buf size i stream)))
      (incr i))
    i))

(defun write (fd buf cnt)
  (declare (external "write"))
  (let ((written (fwrite buf 1 cnt fd))
        (failure (channel-flush fd)))
    (or failure written)))

(defun input-item-nth-char (ptr size item desc i)
  (let ((c (channel-input desc)))
    (if (= c -1) 0
      (memory-write (+ ptr (* size item) i) (cast char c))
      1)))

(defun input-item (buf size item fd)
  (let ((i 0))
    (while (and (< i size)
                (input-item-nth-char buf size item fd i))
      (incr i))
    i))

(defun fread (ptr size n stream)
  (declare (external "fread"))
  (let ((i 0))
    (while (and
            (< i n)
            (= size (input-item ptr size i stream)))
      (incr i))
    i))

(defun read (fd buf n)
  (declare (external "read"))
  (fread buf 1 n fd))

(defun fgetc (stream)
  (declare (external "fgetc" "getc"))
  (channel-input stream))

(defun fgets-step (ptr len str i)
  (let ((c (channel-input str)))
    (if (= c -1) 0
      (memory-write (+ ptr i) (cast char c))
      (not (= c 0xA:8)))))

(defun fgets (ptr len str)
  (declare (external "fgets"))
  (let ((i 0))
    (while (and (< i len)
                (fgets-step ptr len str i))
      (incr i))
    (memory-write (+ ptr (min (-1 len) (+ ptr i))) 0:8)
    ptr))

(defun getchar ()
  (declare (external "getchar"))
  (fgetc *standard-input*))

(defmethod fini ()
  (channel-flush *standard-output*)
  (channel-flush *standard-error*))
