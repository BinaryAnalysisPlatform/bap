(require check-value)

(defmethod call-return (name _ ret)
  (when (is-in name
           'malloc 'getenv 'mkdtemp 'mkstemp
           'posix_openpt 'system 'gets 'fgets 'feof
           'ferror 'fileno 'chdir 'pipe 'setuid)
    (check-value ret)))

(defmethod call-return (name _ _ ret)
  (when (is-in name 'calloc 'realoc 'fdopen 'fopen
               'realpath 'listen 'shutdown 'access 'getcwd
               'symlink 'truncate 'popen)
    (check-value ret)))

(defmethod call-return (name _ _ _ ret)
  (when (is-in name
               'freopen 'strxfrm 'accept 'bind 'connect
               'recvmsg 'sendmsg 'socket 'chown 'readv
               'readlink 'write)
    (check-value ret)))

(defmethod call-return (name _ _ _ _ ret)
  (when (is-in name 'fread 'fwrite 'recv 'send 'socketpair
               'pread 'pwrite 'read)
    (check-value ret)))

(defmethod call-return (name _ _ _ _ _ _ _ ret)
  (when (is-in name 'recvfrom 'sendto 'setsockopt)
    (check-value ret)))


(defmethod call-return (name _ _ _ _ _ _ ret)
  (when (is-in name 'recvfrom 'sendto)
    (check-value ret)))
