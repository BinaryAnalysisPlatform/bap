open Format

(** Controls tags in format strings.

    This module provides facilities to control tags in the format
    strings. For simplicity and readability we're using sexp to define
    tags with attributes. See below for more information.

    The default behavior is to ignore tags. You can switch between
    modes using [install] and [with_mode] function (the last one
    will restore previous state of the formatter).

    Tags allows to literally tag pieces of text in format strings,
    the format of tagging can be explained with the following example:

    [@{<html>@{<head>@{<title>Tags!@}@}@{<body>Hello!@}@}]

    In html mode, this will produce a valid [HTML] document:

    [
      <html>
       <head>
        <title>
        Tags!
        </title>
       </head>
       <body>
       Hello!
       </body>
      </html>
    ]

    In hypothetical [LaTeX] mode it can produce a tex article, and so
    on.

    If tag has attributes, then they should be passed as
    S-expressions, with the following grammar:

    tag = name | "(", name, {arg}, ")";
    arg = "(", name, value, ")".

    Where [name] is arbitrary html valid name without [>] in it.
    The same is true for [value] with an addition of double quotes.
    If [value] start with double quotes it will be stored as is,
    otherwise it will be properly escaped and double quoted.

    {3 Implementation details}

    Tags in [Format] module are stringly typed, so we will use a
    trick, and encode them using s-expressions. Not very typesafe,
    but better than arbitrary string.

    Example: [(a (href #) (id x))] will be converted to [<a
    href="#" id="x">]. Note, that quotes will be inserted
    automatically, and special symbols will be escaped.  If
    attribute value is quoted by itself, then it will be taken as
    is. If tag doesn't contain attributes, then it can be provided
    as an atom, e.g. [html], but more verbose [(html)] is still
    accepted
*)

(** A name of mode, by default the following modes ares supported
    [html], [blocks], [attr] and [none]
*)
type mode = string
exception Unknown_mode of string


val install : Format.formatter -> mode -> unit
val with_mode : Format.formatter -> mode -> f:(unit -> 'a) -> 'a

val register_mode : mode -> (formatter -> unit) -> unit
val available_modes : unit -> mode list


module Attr : sig
  val show : string -> unit
  val hide : string -> unit
  val print_colors : bool -> unit
end
