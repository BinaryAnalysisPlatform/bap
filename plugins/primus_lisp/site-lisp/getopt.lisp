(require type)
(require array)
(require strstr)

(in-package posix)
(declare (visibility :private))


(declare (global (optind opterr optopt optarg)))

(defun getopt-arg (argv)
  (array-get ptr_t argv optind))

(defun getopt-arg-char (argv n)
  (array-get byte (getopt-arg argv) n))

(defun points-to-dash (p)
  (points-to char_t p ?-))
(defun points-to-colon (p)
  (points-to char_t p ?:))

(defun getopt-finished (argc argv)
  (or (> optind argc)
      (is-zero (getopt-arg argv))
      (not (points-to-dash (getopt-arg argv)))
      (points-to-null (getopt-arg argv))))

(defun getopt-nearly-finished (argv)
  (let ((p (getopt-arg argv)))
    (and (points-to-dash (ptr+ p 1))
         (points-to-null (ptr+ p 2)))))

(defun getopt-update-optopt (argv last-ofs)
  (set optopt (getopt-arg-char argv (+1 last-ofs))))

(defun getopt-update-optarg (argv)
  (set optarg (array-get ptr_t argv (+1 optind))))

(defun getopt-found (argv p last-ofs)
  (or (points-to-colon (ptr+ 2 p))
      (getopt-arg-char argv (+ 2 last-ofs))))

(defun getopt-reset-optarg-if-needed (argv last-ofs)
  (when (points-to-null (getopt-arg-char argv (+ 2 last-ofs)))
    (set optarg 0)))

(defun getopt-expects-argument (p)
  (points-to-colon (ptr+ 1 p)))

(defun getopt-missing-argument ())

(defun getopt-no-argument (argv opts)
  (incr optind)
  (when (not (points-to-colon opts))
    (getopt-missing-argument))
  ?:)

(defun getopt-with-argument (argv opts p last-ofs)
  (if (getopt-found argv p last-ofs)
    (getopt-reset-optarg-if-needed argv last-ofs))
  (prog
   (getopt-update-optarg argv)
   (when (is-zero optarg)
     (getopt-no-argument argv opts))
   (incr optind)))

(defun getopt (argc argv opts)
  (declare
   (visibility :public)
   (external "getopt"))
  (when (= 0 optind)
    (set optind 1)
    (set lastidx 0))
  (if (getopt-finished) -1
    (if (getopt-nearly-finished argv)
        (prog (incr optind) -1)
      (let ((p 0) )
        (getopt-update-optopt argv last-ofs)
        (set p (strchr opts optopt))
        (if p
            (prog
             (when (points-to-null p)
               (prog (incr optind) (getopt argc argv opts)))
             (if (getopt-expects-argument p)
                 (getopt-with-argument argv opts p last-ofs)
               (prog (incr last-ofs) optopt)))
          (prog
           (getopt-unknown-option)
           (incr opting)
           ??))))))
