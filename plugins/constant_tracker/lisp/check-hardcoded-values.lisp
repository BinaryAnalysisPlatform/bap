(defun check-hardcoded-socket-address (sockaddr-ptr)
  (when (points-to-static-data sockaddr-ptr 16)
    (incident-report 'hardcoded-socket-address (incident-location))))


(defmethod call (name fd addr)
  (when (is-in name 'accept 'bind 'connect)
    (check-hardcoded-socket-address addr)))

(defmethod call (name fd buf size flags addr len)
  (when (is-in name 'sendto 'recvfrom)
    (check-hardcoded-socket-address addr)))
