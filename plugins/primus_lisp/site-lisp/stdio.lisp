(require libc-init)
(require memory)
(require types)

(in-package posix)

(defun fputc (char stream)
  (declare (external "fputc" "putc" "fputs_unlocked putc_unlocked"))
  (if (= 0 (channel-output stream char)) char -1))

(defun putchar (char)
  (declare (external "putchar" "putchar_unlocked"))
  (fputc char *standard-output*))

(defun fputs (p stream)
  (declare (external "fputs" "fputs_unlocked"))
  (while (not (points-to-null p))
    (fputc (cast int (memory-read p)) stream)
    (incr p))
  (let ((r (fputc 0xA stream)))
    (fflush stream)
    r))

(defun puts (p)
  (declare (external "puts" "puts_unlocked"))
  (fputs p *standard-output*))

(defun fflush (s)
  (declare (external "fflush" "fflush_unlocked"))
  (channel-flush s))



;; the channel module have rough equality between streams and
;; file descriptors, as they both are represented as integers. We are currently
;; ignoring modes, we will add them later, of course.
(defun fopen (path mode)
  (declare (external "fopen" "open" "fdopen"))
  (let ((file (channel-open path)))
    (if (< file 0) 0 file)))

(defun fileno (stream)
  (declare (external "fileno"))
  stream)

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
  (declare (external "fwrite" "fwrite_unlocked"))
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
  (declare (visibility :private))
  (let ((c (fgetc desc)))
    (if (= c -1) 0
        (memory-write (+ ptr (* size item) i) (cast char c))
        1)))

(defun input-item (buf size item fd)
  (declare (visibility :private))
  (let ((i 0))
    (while (and (< i size)
                (input-item-nth-char buf size item fd i))
      (incr i))
    i))

(defun fread (ptr size n stream)
  (declare (external "fread" "fread_unlocked"))
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
  (declare (external "fgetc" "getc" "fgetc_unlocked" "getc_unlocked"))
  (channel-input stream))

(defun terminate-string-and-return-null (ptr)
  (declare (visibility :private))
  (memory-write ptr 0:8)
  0)

(defun fgets (ptr len str)
  (declare (external "fgets" "fgets_unlocked"))
  (if (= len 0) (terminate-string-and-return-null ptr)
    (let ((i 0)
          (n (-1 len))
          (continue true))
      (while (and continue (< i n))
        (let ((c (fgetc str)))
          (if (= c -1)
              (set continue false)
            (memory-write (+ ptr i) (cast char c))
            (set continue (/= c 0xA))
            (incr i))))
      (if (= i 0)
          (terminate-string-and-return-null ptr)
      (memory-write (+ ptr (min n i)) 0:8)
      ptr))))


(defun getchar ()
  (declare (external "getchar" "getchar_unlocked"))
  (fgetc *standard-input*))

(defmethod primus:machine-kill ()
  (channel-flush *standard-output*)
  (channel-flush *standard-error*))
