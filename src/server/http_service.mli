(** Implements transport layers on top of HTTP.

    {2 Service provider}

    {3 Data formats}

    This section describes HTTP protocol specific details of the
    service provider.

    Requests starting with prefix [get] MUST use method [GET]. All other
    requests MUST use method [POST]. The [Content-Type] header MUST be
    present and MUST be set to "application/json". The presence and
    content of all other header are regulated by RFC2616.

    The JSON request MUST be contained in a message-body part of the
    request.
    The response MUST contain message-body if the reponse status is
    "200 OK", otherwise it MAY not contain message-body.

    Note: status "OK" indicates only that transport layer has
    succesfully served the request and provided the response. But the
    response by itself can still indicate an error.


    {3 Session}

    In order to implement streaming ontop of HTTP the Client/Server
    interaction should convey to the following rules.

    1. Session MUST be started by the client side with a [PUT]
    request, containing [Pragma] header with value [start-session].

    2. Client MUST wait for the response on the initial request, in
    order to obtain session id. The session id MUST be a positive
    number lesser then max_sessions. If server cannot allocate such
    unique number it MUST respond with an error.

    3. Server MUST respond on the initial request with one HTTP
    response, that MUST contain [Pragma] header with a unique
    identifier of the session, and chunked body-message. The response
    MUST contain [Transfer-Encoding] header with value equal to
    ["chunked"].

    4. Client MAY send further requests to the same connection
    (established by the initial request) or create new connections to
    send new requests. All requests MUST contain [Pragma] header with
    value either equal to session id, or [start-session], to create
    new session.

    5. Server MUST respond to all request, other than the requests
    containing [Pragma : start-session] header, to the stream,



*)
