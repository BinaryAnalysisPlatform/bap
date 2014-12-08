(** shares files using mmaping.


    mmap server and clients doesn't actually transfer data between,
    but share the data using URL to address a piece of data in the
    filesystem.

    The server responsibility is to create files for memory chunks
    if they are unowned, i.e., doesn't have a file.

    The URI format is used in a following way:

    - [scheme] must be [mmap]
    - [path] must point to an existing file
    - [query] should contain two fields:
      - [offset] an offset in bytes from the start of the file
      - [length] length of the memory chunk


    When server accepts a new memory chunk with the [field] parameter
    pointing to a valid mmapable file it will assume, that
    [Bigsubstring.base] refers to a file data, and return a correct
    url pointing to the memory chunk in the specified data. If [file]
    parameter is left uspecified, then server will look at the base of
    a bigstring and if it is already mapped it before, the it will
    reuse it, otherwise it will create a file and return an url
    pointing to it. If the query parameter is not None, then it will be
    appended to the query with the key [q].

    [mmap:///bin/ls?offset=16&length=1024&q=user]

*)
