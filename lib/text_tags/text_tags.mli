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

    ["@{<html> @{<head> @{<title> Tags!@}@}@{<body> Hello!@}@}"]

    In html mode, this will produce a valid [HTML] document:

    {v
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
    v}

    {2 none mode}

    In this mode (the default one) the tags will be ignored, and they
    will have no effect on the final output.


    {2 html mode}

    If tag has attributes, then they should be passed as
    S-expressions, with the following grammar (EBNF):

    {v
    tag = name | "(", name, {arg}, ")";
    arg = "(", name, value, ")".
    v}

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


    {2 Attr mode}

    In this mode the tag has the following syntax:

    {v tag = ".", name, value, "\n" v}

    where name is a strings that must not contain whitespaces. (Usually
    we use dashes to separate words), and value is an arbitrary
    sequence of chars that must not contain the newline character.

    The attributes will be printed as is, if the mode is enabled, and
    a name of the attribute is marked to be shown with {!Attr.show}
    function (by default no attributes will be shown).


    {3:colors Ansi color submode}

    When {!Attr.print_colors} is set to [true], the [foreground] and
    [background] attributes will be handled specially. The attribute
    value, that must be a valid ansi color escape sequence, will
    switch the foreground and background colors of the text, if the
    terminal allows it (otherwise ugly things will happen, that's why
    the attribute is ignored when color submode is disabled).


    {2 Blocks mode}

    The blocks mode grammar is very similar to html mode, as it also
    uses sexp syntax.

    {v tag := "(", "id", value, ")" | "(", "title", value, ")" v}

    Where [value] is a string, delimited with quotes if it contains
    whitespaces.

    The tag will be rendered as begin/end block, with the body
    indented by one space. E.g.,


     Will be rendered as:

     {v
      begin(main)
       r0 := r1 + r2
       r1 := r2 + r2
       return lr
      end(main)
      v}

    If both [id] and [title] is specified, then only title will be
    outputted. Otherwise, [id] and [title] has the same behavior.
*)

(** A name of mode, by default the following modes ares supported
    [html], [blocks], [attr] and [none]
*)
type mode = string
exception Unknown_mode of string



(** [install ppf mode] switch formatter [ppf] into a [mode].
    In a default [mode] (named [none]), the semantics tags are
    ignored. Once a [mode] is installed, all tags will be rendered
    according to the mode.
  *)
val install : Format.formatter -> mode -> unit


(** [with_mode ppf mode f] installs [mode], calls [f], and then
    reinstalls the previous mode.  *)
val with_mode : Format.formatter -> mode -> f:(unit -> 'a) -> 'a


(** [register_mode mode init] installs new mode.
    The [init] function must install all mode hooks using
    {!Format} interface.  *)
val register_mode : mode -> (formatter -> unit) -> unit


(** [available_modes ()] lists all currently installed modes.  *)
val available_modes : unit -> mode list

(** Attributes mode.

    See {!colors} for the description of the mode.
*)
module Attr : sig

  (** [show name] enable attribute with the [name].  *)
  val show : string -> unit

  (** [hide name] disable attribute with the [name].  *)
  val hide : string -> unit

  (** [print_colors yesno] enable/disable color submode. *)
  val print_colors : bool -> unit
end
