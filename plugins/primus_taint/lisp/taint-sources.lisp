(require incident)

(defun untrust/value (v)
  "taints V with as 'untrusted unless it is already 'untrusted"
  (when (not (taint-get-direct 'untrusted v))
    (dict-add 'taint-sources/untrusted
              (taint-introduce-directly 'untrusted v)
              (incident-location))))

(defun untrust/region (p n)
  "taints a memory region that is pointed by P and has the length N if
   it wasn't tainted yet"
  (when (not (taint-get-indirect 'untrusted p))
    (dict-add 'taint-sources/untrusted
              (taint-introduce-indirectly 'untrusted p n)
              (incident-location))))

(defun untrust/string (p)
  "taints a string pointed by P (except the terminating null)"
  (untrust/region p (strlen p)))

;; Rule:
;; r = atoi(a), r = atol(a), r = atoll(a) |- T(r), T(a,strlen(a))
;;
;; Motivation:
;; The textual representation of numeric data as an
;; indicative of some text based communication protocol.
(defmethod call-return (name arg ret)
  (when (is-in name 'atoi 'atol 'atoll)
    (untrust/region arg (strlen arg))
    (untrust/value ret)))

;; same as above but for strtod, strtol, and strtold
(defmethod call-return (name arg _ ret)
  (when (is-in name 'strtod 'strtol 'strtold)
    (untrust/string arg)
    (untrust/value ret)))

;; Rule:
;; r = getenv(_) |- T(r,strlen(r))
;;
;; Motivation:
;; though in many contexts environment variables could be considered
;; trusted, in some they are not, in particular in CGI, when
;; environment is used to pass user data.
(defmethod call-return (name _ ret)
  (when (= name 'getenv)
    (untrust/string ret)))

;; Rule:
;; realpath(x,y,n) |- T(x,strlen(x)), T(y,strlen(x))
;;
;; Motivation:
;; If a program sanitizes a path, then it indicates that the program
;; itself doesn't trust it, so it came from an untrusted input.
(defmethod call-return (name input output)
  (when (= name 'realpath)
    (untrust/string output)
    (untrust/region input (strlen input))))

;; Rule:
;; r = system(x) |- T(r)
;;
;; Motivation:
;; An adversary may affect the program environment and control the
;; exit code of trusted programs
(defmethod call-return (name cmd ret)
  (when (= name 'system) (untrust/value ret)))

;; Rule:
;; r = read(_,p,n) |- T(p,n), T(r)
;;
;; Motivation:
;; Data came from the IO operation
(defmethod call-return (name _ buf len res)
  (when (= name 'read)
    (untrust/region buf len)
    (untrust/value res)))

;; Rule:
;; r = pread(_,p,n,_) |- T(p,n), T(r)
;;
;; Motivation:
;; Same as with read
(defmethod call-return (name _ buf len off res)
  (when (= name 'pread)
    (untrust/region buf len)
    (untrust/value res)))

;; Rule:
;; fgets(p,n,_) |- T(p,n)
;;
;; Motivation:
;; Same as with read.
(defmethod call (name buf len)
  (when (= name 'fgets)
    (untrust/region buf len)))

;; Rule:
;; r = fread(p,n,m,_) |- T(p, n*m), T(r)
;; Motivation:
;; Same as with read
(defmethod call-return (name p n m ret)
  (when (= name 'fread)
    (untrust/region p (* n m))
    (untrust/value ret)))

;; Rule:
;; m = recv(_,p,n) |- T(p,n), T(m)
(defmethod call-return (name fd buf len flgs ret)
  (when (= name 'recv)
    (untrust/region buf len)
    (untrust/value ret)))

;; Rule:
;; recvfrom(_,p,n,...)
(defmethod call (name fd buf len flg src slen res)
  (when (= name 'recvfrom)
    (untrust/region buf len)
    (untrust/value res)))

;; Rule:
;; x = fgetc(_), x = getc(_) |- T(c)
;; Motivation:
;; User input
(defmethod call-return (name _ ret)
  (when (is-in name 'fgetc 'getc)
    (untrust/value ret)))


;; Rule:
;; str?cmp(x,y), strcoll(x,y), str?casecmp(x,y) |- T(x,strlen(x)), T(y,strlen(y))
;; Motivation:
;; A comparison of two strings indicates that at least one of the strings is not statically
;; known, thus it should come from source, external to the program.
(defmethod call (name s1 s2)
  (when (is-in name 'strcmp 'strcasecmp 'strcoll 'strncasecmp 'strncmp)
    (untrust/string s1)
    (untrust/string s2)))

;; Rule:
;; r=str?cmp(x,y), r=strcoll(x,y), r=str?casecmp(x,y) |- T(r)
;; Motivation:
;; The result of string comparison is also controlled by untrusted input.
(defmethod call-return (name s1 s2 res)
  (when (is-in name 'strcmp 'strcasecmp 'strcoll)
    (untrust/value res)))

;; Rule:
;; r = crypt(x,_) |- T(r), T(x,strlen(x))
;; Motivation:
;; Crypt is often used for checking the authentication token that comes
;; from the untrusted party (otherwise why would we check it)
(defmethod call-return (name key)
  (when (= name 'crypt)
    (untrust/region key (strlen key))))

;; Rule:
;; r = readlink(p,q,n) |- T(p,strlen(p)), T(q,n), T(r)
;; Motivation:
;; Readlink reads a contents of a link, that could be arbitrary,
;; as well as having arbitrary size
(defmethod call-return (name path buf len res)
  (when (= name 'readlink)
    (untrust/region path (strlen path))
    (untrust/region buf len)))

;; Rule:
;; swab(x,y,n) |- T(x,n), T(y,n)
;; Motivation:
;; Changin byteorder often indicates that data is in the network ordering,
;; thus it came from an untrusted source (it could also indicate the opposite,
;; that data is sent to network).
(defmethod call (name src dst len)
  (when (= name 'swab)
    (untrust/region src len)
    (untrust/region dst len)))


;; Rule:
;; sscanf(s,_, ...) |- T(s,strlen(s))
;; Motivation:
;; Do not trust data that require parsing
(defmethod call (name str)
  (when (= name 'sscanf)
    (untrust/string str)))
