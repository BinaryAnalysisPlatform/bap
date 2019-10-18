(** The entry point to BAP.

    This module is an entry point to BAP and serves the two goals:

    - embedding BAP in other applications;
    - extending BAP with the user code.

    {2 Embedding BAP}

    BAP is designed to be friendly and act as a library, so that it
    can be seamlessly embedded into user applications, in cases when
    the main frontend of BAP, the [bap] utility, couldn't suffice the
    user requirements. Being a guest, BAP will act respectfully to its
    host and won't interfere with system utilities, unless allowed to,
    e.g., it won't terminate the program, hijack the control flow,
    spam into channels and, in general, will keep quiet and minimize
    possible side-effects.

    Embedding is achieved by a simple call to the [Bap_main.init ()]
    procedure which takes a few optional arguments. By default it will
    just initialize plugins, peeking configuration from the predefined
    locations and environment variables (which in turn, could be also
    specified). If command line arguments are passed, the [init]
    procedure will evaluate them. See the [bap] utility for the
    description of the command line interface and semantics of command
    line arguments.

    {3 Warning}

    Sine BAP is relying on dynamic loading, for correct behavior the
    host program shall provide information to the dynamic loader
    about the units that are linked into the host program. Failure to do
    so may result in an undefined behavior with the segmentation fault
    being the most favorable outcome.

    This requirement could be achieved by using the [ocamlfind]
    tool to build the host program and specifying
    [-package findlib.dynload] in the linking command [1].

    Alternatively, if [dune] is used, then adding [findlib.dynload] to
    the libraries dependencies of the host application (e.g.,
    [(libraries findlib.dynload)] should also work) [2].

    Finally, if neither of approaches suffice, the dependencies could
    be manually set using the [Findlib.record_package] function.

    [1]: http://projects.camlcity.org/projects/dl/findlib-1.5.6/doc/ref-html/lib/Fl_dynload.html
    [2]: https://jbuilder.readthedocs.io/en/latest/advanced-topics.html#dynamic-loading-of-packages



    {2 Extending BAP}

    It is much more common and recommended to use the [bap] utility to
    initialize and run BAP. The user code could be injected in
    predefined extension points, and will be called by the system with
    all the necessary input parameters. This approach minimizes the amount of
    the boilerplate that has to be written and lets an analyst to
    inject its analysis in the right place of a pipeline.

    There are plenty of extension points in BAP, too many to mention
    here, but writing a disassemling pass would be a good
    example. Using the [Project.register_pass] function an analyst can
    get straight to the point and apply its analysis as a
    transformation to the [project] data structure without being
    obligated to create this structure on the first hand, thus
    relinquishing to the BAP framework the responsibility of parsing
    the command line arguments, selecting proper loaders and disassembler
    parameters.

    This approach also establishes a unified inteface to BAP making the
    whole system easier to use and understand.

    {3 Plugins}

    A plugin is compiled and packed code that could be loaded in
    runtime. A plugin is a bundle that in addition to the machine and
    byte code of the extension itself, contains the meta information
    that descibes plugin properties, requirements, and provided
    features. It may also optionally contain the code for
    dependencies, which leverages plugin portability, so that it can
    be loaded when the development environment is no longer
    available. (Note, by default all dependencies except the bap
    library itself and core_kernel) are packed into the plugin.

    The [bapbuild] tool is used to build plugins from OCaml source
    code. The [bapbundle] tool could be used to deploy the plugin into
    a place where it will be automatically loaded by the framework. In
    short, to build and deploy a plugin with OCaml code located in a
    file named [example.ml] execute the following two commands:

    1. [bapbuild example.plugin]
    2. [bapbundle install example.plugin]

    The [bapbuild] tool will scan the dependencies of the [example.ml]
    file and build them automatically if they are present in the
    current directory, e.g., if [example.ml] references the [Analysis]
    module and [analysis.ml] is present in the current folder, then it
    will be automatically built and linked into the final product. A
    dependency on an external package could be specified using the
    [-pkg] and [-pkgs] option (the latter accepts a comma separated
    list of dependencies). Underneath the hood, [bapbuild] is the
    standard OCaml [ocamlbuild] tool extended with a few rules that
    are necessary to build and pack plugins.

    The [bapbuild] tool has its limitiations, for example, only one
    plugin per folder could be built. When the source base grows very
    big it is becoming hard to manage it with [bapbuild], so using
    some configuration system is advised, e.g., OASIS or dune. A
    plugin, then, could be built as a normal OCaml library and later
    packed with [bapbuild].

    {3 Extensions}

    After the plugin is deployed to the place where it could be found
    by BAP, it will be loaded every time the [Bap_main.init] function
    is called. All toplevel expressions of all modules consituting the
    plugin will be evaluated, however, a well-behaving plugin shall
    not evaluate any side-effectful expressions except those that are
    provided by the [Extension] module.

    The [Extension] module let the extension to

    1) declare configuration parameters;
    2) declare command line arguments;
    3) declare an extension;
    4) declare a command;
    5) specify meta attributes such as documentation, features, and
    requirements.

    When an extension is enabled by the framework (see the {!features}
    section which describes the process of selection), it will be
    evaluated with the context, capturing the computation environment,
    passed to it as function argument.

    {3 Commands}

    Commands are special kinds of extensions which stand aside because
    the play the role of the [main] function in BAP, i.e., a command
    is an OCaml function which will be evaluated as the main function,
    when BAP is run.

    Commands can have their own command line arguments, which are then
    reified into OCaml values and passed to the specified function as
    arguments.

    {2 Features and Requirements}

    BAP employs a system of simple sematic tags to denote required and
    provided capabilities of its various components. This system
    facilitates fine granular selection of components that are
    required for an application to satisfy it needs.

    {3 Features}

    Both the main system and its extensions may explicitly state the
    set of features that they provide or expect, as well as the set
    of requirements that they require or implement.

    Both, features and requirements are intentionally denoted with
    string tags with no specific requirements.

    A feature is a high-level description of an application and its
    environment. It is used to describe to the extensions what the
    application is doing and what should be expected by an extension.

    Features are specified by the application via the [init]
    function. An extension may define a specific set of features that
    it expects to be present and won't be loaded by application which
    do not specify the expected features.

    Some general examples of features are [user-interface],
    [interactive], [toplevel].

    Another common use case of features is denoting an tag that is
    specific to the given application or an organization, e.g.,
    [my-verification-framework] or [cmu.edu], and specify them in
    plugins to ensure that they are loaded only in the specified
    environments, but not in more general.

    The more features an application specifies the more general it is,
    i.e., more extensions will be available for it. The more features
    an extension specifies, the less general it is, i.e., it could be
    used in fewer applications.

    The list of features known to the [bap] utility, could be obtained
    by using the [bap list features] command.

    {3 Requirements}

    The requirements are more fine granular descriptions of system
    capabilities that are used to define system dependencies without
    relying on concrete implementations. For example, if an application
    needs to parse ELF files it may explicitly define this dependency
    by adding the [elf] tag to the list of its requirements.

    By using requirements in this manner it is possible to build an
    application that loads some minimal set of dependencies.

    {3 Caching}

    Requirements are also playing an important role in the caching
    subsystem and in general leverage reproducibility of BAP
    applications by enabling pure functional relationships between BAP
    components.

    Every BAP extension is evaluated in the context, which is captured
    by a value of type [ctxt] that is passed to each extension
    function. The context is an immutable value that fully describes
    the set of configuration parameters, command line arguments, and
    other descriptors of the environment in which the BAP subsystem is
    evaluating.

    It is possible to reduce the context into its cryptographic
    digest, which, in turn, could be used as key in some persistent
    storage, which, useful for implementing caching. However,
    computing a digest of the whole context could be overconservative,
    since it may also capture variables that are irrelevant to a given
    computation. For that reason, we provide a mechanism to refine the
    context by specifying a set of tags that relate the computation to
    the environment.

    For example, the disassembler command, provided by the
    [disassemble.plugin] depends on a predefined set of features
    provided by different plugins, namely, [disassembler], [lifter],
    [symbolizer], [rooter], [reconstructor], [brancher], and
    [loader][^1]. Therefore, it depends on extensions that provide
    those features, and when parameters of those extensions change,
    it is reflected by the context refinement that the disassemble
    plugin is using to compute the key for storing the disassembled
    program in the cache storage.

    In other words, it is important to specify explicitly features of
    your extensions, to ensure that any change in their configuration
    is reflected and propagated to the components that may depend on
    your extension.

    Use the [bap list tags] command to list all semantics tags and
    plugins that provide them.

    [^1]: The list is not definitive and may change, consult the
    plugin documentation for the exhaustive and up-to-date list.
*)

open Bap_future.Std

(** describes an error condition. *)
type error = ..


(** captures the evaluation context. *)
type ctxt


(** [init ()] initializes the BAP framework.

    Attention: function is only needed when BAP framework is
    embedded in another application. It shall not be called in BAP
    plugins. If you're not sure whether you need to call it, then
    don't call it.

    The [init ()] expression evaluates to [Ok ()] if the system
    initialization terminated normally and is fully complete. It returns
    [Error condition] in case if the evaluation terminated abnormally
    with the [condition] value that describes the reasons and
    consequences of this abnormal termination (note, despite the name,
    it is not always an error, e.g., a user may have requested the
    help message, using the --help command).

    If [init ()] terminates with any value other thatn [Ok ()] the
    BAP framework is considered to be unitialized and shouldn't be
    used.

    The initialization procedure uses the provided parameters to
    evaluate command line and environment arguments, loads the
    requested plugins and dispatches commands if any are requested
    through the command line.

    This function could be invoked only once per lifetime of a
    process, and consecutive

    @parameter features, if specified, denotes a set of features of an
    application that extensions can expect. Extensions that require a
    feature which is not in the [features] provided by the application
    will not be evaluated.

    @parameter requires if specified then only those extensions that
    provide at least one feature in [requires] will be evaluated.

    @parameter library specifies a list of folders that will be
    prepended to the plugins search paths list (which already contains
    some precompiled location and the value of the BAP_PLUGIN_PATH
    environment variabe, which in turn could also be a list).

    @parameter argv is the array of command line arguments, with the first
    value being the program name. If not specified, then it defaults
    to [[|Sys.progname|]], i.e., no command line arguments will be
    evaluated. If you want to let [init] process the command line
    passed to the process, use the [Sys.argv] variable.

    @parameter env, if specified, then this function will be used to
    access environment variables. Otherwise, the environment varaibles
    are looked up using the [Sys.getenv] function.

    @parameter log, if specified, then the specified location will be
    used for logging. If [`Formatter ppf] is passed then all log
    messages will be printed into [ppf] (every message is flushed).
    If [`Dir path] is passed, then all log messages will be printed in
    the [Filename.concat path "log"] file. If such file exists, then
    it will be renamed to "log~1", if "log~1", in turn, exists, it
    will will be renamed to "log~2" and so on, until "log~99" is
    reached, which will be discarded. If the [log] parameter is not
    specified, then the logging will be performed in a directory which
    name is obtained either from the command line (via the [--logdir]
    parameter) or from the environemt (using the [BAP_LOG_DIR]
    variable). If neither is present then the logging will be
    performed into a directory prescribed by the XDG standard for the
    application - i.e., to the `$XDG_STATE_HOME/bap`, where the
    environment variable [XDG_STATE_HOME] defaults to
    [$HOME/.local/state]. If, for some reason, it wasn't possible to
    create a log file, then logging will fallback to the [stderr]
    channel. Note, a usual log rotating routine will be applied in the
    log directory, as described above.

    @parameter out if specified, then this channel will be used to report help
    and other informational messages, if such are requested through
    command line.

    @parameter err if specified, then this channel will  be used to report error
    and other diagnostic messages in case of configuration
    problems. Nothing will be printed in this channel if the
    initialization procedure went normally (and evaluated to [Ok ()]).

    @parameter man is the manual describing the purposes and
    basic usage of the utility in which bap is embedded. It is useful
    if the host program is going to use BAP command line parsing
    facilities, so it will be rendered when the [--help] option is
    specified. A simple markdown syntax is understood, i.e.,
    paragraphs, section headers, itemized lists, and verbatim code
    sections.

    @parameter name is used as the name of the process. If not
    specified, then [Sys.progname] is used.

    @parameter version defaults to the BAP Framework version.

    @parameter default, if specified, then this function will be invoked
    when no command was specified in the command line.
*)
val init :
  ?features:string list ->
  ?requires:string list ->
  ?library:string list ->
  ?argv:string array ->
  ?env:(string -> string option) ->
  ?log:[`Formatter of Format.formatter | `Dir of string] ->
  ?out:Format.formatter ->
  ?err:Format.formatter ->
  ?man:string ->
  ?name:string ->
  ?version:string ->
  ?default:(ctxt -> (unit,error) result) ->
  unit -> (unit, error) result


(** Writing and declaring BAP extensions.  *)
module Extension : sig


  (** defines a data type for a parameter.  *)
  type 'a typ


  (** [declare extension] declares the [extension] function.

      The function is run when one of the features [provided] by the
      [extension] is required by the call to [Bap_main.init] and all
      [features] needed by the extension are present.

      If [extension] evaluates to [Error condition], then no other
      extensions will be evaluated and the initilialization procedure
      will stop immediately with the [Error condition] as the final
      result.

      If [extension] raises an exception [e], then it will be caught, and
      the initialization procedure will terminated immediately with
      the [Error (Error.Bug (e,backtrace)].

      @parameter features is a set of application features that the
      declared extension expects. If there is a feature that is
      required but not provided by an application, then the extension
      will not be evaluated. See the corresponding [features]
      parameter of the [init] function, as well as the {!features}
      section.

      @parameter provides is a set of features that the extension
      provides. During the initialization, only those extensions that
      provide features that are requested by the application are
      loaded. All configuration parameters of the extension will be
      attributed with tags from the [provides] set, so that they will
      only affect those components that explicitly depend on on of the
      specified features. Note that, the set of provided features is
      shared by all extensions of a plugin, in other words it is an
      attribute of a plugin rather than of a particular extension
      function.

      @parameter doc is the documentation provided with the
      extension. It can take a form of a one line description or a
      full manual written in the markdown syntax. See the
      corresponding [man] parameter of the [Bap_main.init] function.
  *)
  val declare :
    ?features:string list ->
    ?provides:string list ->
    ?doc:string ->
    (ctxt -> (unit,error) result) -> unit



  (** [documentation s] specifies plugin documentation.

      A non-declarative way of specifying documentation. Each
      occurence of the [documentation s] appends [s] to the plugin
      documentation, as well as each occurence of the [doc] parameter
      of the [Extension.declare] function.

      See the [doc] parameter of the {!Extension.declare} and
      !{Bap_main.init} functions for more information on the accepted
      formats.
  *)
  val documentation : string -> unit

  (** Interface for specifying commands.*)
  module Command : sig

    (** denotes the command signature.

        The ['f] parameter is the type of function that is evaluated
        when the command is selected.

        The ['r] is the value returned by ['f] (and so ['r] is always
        included in ['f].

        For example,
        {[
          type s =
            (int -> ctxt -> (unit,error) result,
             ctxt -> (unit,error) result) t
        ]}

        is a type of a function that takes two input arguments of type
        [int] and [ctxt] respectively, and must evaluate to a value of type
        [unit,error] result
    *)
    type ('f,'r) t


    (** ['a param] command line parameter represented with the OCaml
        value of type ['a].  *)
    type 'a param

    (** [declare signature name command] declares a [command].

        Declares to BAP that a command with the given [name] should be
        invoked when a user specifies the [name] or [:name] as the
        first argument in the command line. The [signature] defines
        the command line grammar of this command.

        When the command is selected and command line arguments are
        parsed succesfully, the [command] function is applied to the
        the passed (and parsed to their OCaml representation) command
        line arguments. The result of evaluation of the [command] will
        become the result of the [Bap_main.init ()] expression in the
        host program.

        If the function with the given [name] is already registered,
        then the command is not registred and BAP initialization will
        terminate abnormally with the configuration error condition.

        {3 Examples}

        Note, the examples below assume the following preamble:
        {[
          open Core_kernel
          open Bap_main.Extension
        ]}

        1) Declare a command with no arguments:

        {[

          let () =
            Command.(declare "hello" args) @@
            fun ctxt ->
            printf "the `hello' command is called\n";
            Ok ()
        ]}


        2) Declaring a command with one positional argumet

        {[
          let input = Command.argument Type.int
          let () =
            Command.(declare "hello" (args $input)) @@
            fun input ctxt ->
            printf "called as `hello %d'\n" input
        ]}

        3) Declaring a command with an optional named
        parameter, and many positional arguments.

        {[
          let inputs = Command.arguments Type.string
          let output = Command.parameter Type.string "output"
          let () =
            Command.(declare "copy" (args $output $inputs)) @@
            fun output inputs ->
            printf "copying %s inputs to %s\n"
              (String.concat ~sep:" " inputs) output
        ]}


        @parameter doc defines the documentation for the declared
        command, it could be as simple one-line description or a full
        featured manual in the markdown syntax. See the corresponding
        parameter in the {!Bap_main.init} function for the description
        of the accepted.

        @parameter requires defines the set of features that are
        required by the implementation of this command. It defaults to
        the set of all possible features. The context value passed to
        the [command] function will be refined to the context of
        extensions which are providing the specified features, i.e.,
        between different invocations of BAP it will not change if
        the configuration parameters of extensions on which the
        command depends didn't change.
    *)
    val declare :
      ?doc:string ->
      ?requires:string list -> string ->
      ('f,ctxt -> (unit,error) result) t -> 'f -> unit


    (** [args] is an empty list of arguments.*)
    val args : ('a, 'a) t


    (** [xs $ x] appends parameter [x] to the list of parameters [xs]  *)
    val ($) : ('a,'b -> 'c) t -> 'b param -> ('a,'c) t


    (** [arguments t] declares a positional argument of type [t].



        Grammar:
        {v
          term ::=
               ...
               <t>
        v}

    *)
    val argument :
      ?docv:string ->
      ?doc:string ->
      'a typ -> 'a param


    (** [arguments t] declares an infinite number of positional
        arguments of type [t].

        Grammar:
        {v
          term ::=
               ...
               <t>, <t>
        v}


        Note, no positional arguments could be appended to the command
        line specification after this one, e.g.,  the following is not
        well formed:

        {[
          (* Warning! Ill-formed code *)

          let inputs = Command.arguments Type.string
          let output = Command.parameter Type.string "output"
          let () =
            Command.(declare "copy" (args $inputs $output)) @@
            fun output inputs ->
            printf "copying %s inputs to %s\n"
              (String.concat ~sep:" " inputs) output
        ]}
    *)
    val arguments :
      ?docv:string ->
      ?doc:string ->
      'a typ -> 'a list param


    (** [switch values name]

        Grammar:
        {v
          term ::=
               ...
               | --<(name v1)>
               | --<(name v2)>
               ...
               | --<(name vN)>
         v}

        where [v1] .. [vN] are elements of [values]

    *)
    val switch :
      ?doc:('a -> string) ->
      'a list ->
      ('a -> string) ->
      'a option param


    (** [switches values name]

        Grammar:
        {v
          term ::=
               ...
               | --<(name v1)>
               | --<(name v2)>
               ...
               | --<(name vN)>
         v}

        where [v1] .. [vN] are elements of [values]

    *)
    val switches :
      ?doc:('a -> string) ->
      'a list ->
      ('a -> string) ->
      'a list param

    (** [dictionary keys t name]

        Grammar:
        {v
          term ::=
               ...
               | "--name(k1)" "=" <t>
               | --<"name(k2)"> "=" <t>
               ...
               | --<"name(kN)"> "=" <t>
         v}

        where name(k1)][k1] .. [kN] are elements of [keys]

    *)
    val dictionary :
      ?docv:string ->
      ?doc:('k -> string) ->
      ?short:('k -> char) ->
      ?as_flag:('k -> 'd) ->
      'k list ->
      'd typ ->
      ('k -> string) ->
      ('k * 'd) list param

    (** [parameter t name]

        Grammar:
        {v
          term ::=
               ...
               | --"name" "=" <t>
         v}

        where [k1] .. [kN] are elements of [keys]

    *)
    val parameter :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      'a typ ->
      string -> 'a param

    val parameters :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?short:char ->
      string ->
      'a typ -> 'a list param

    val flag :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string ->
      bool param

    val flags :
      ?docv:string ->
      ?doc:string ->
      ?short:char ->
      string ->
      int param

  end

  module Parameter : sig
    type 'a t

    val get : ctxt -> 'a t -> 'a

    val declare :
      ?as_flag:'a ->
      'a typ -> ?deprecated:string -> ?default:'a ->
      ?docv:string -> ?doc:string -> ?synonyms:string list ->
      string -> 'a t

    val declare_list :
      ?as_flag:'a ->
      'a typ -> ?deprecated:string -> ?default:'a list ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->  string -> 'a list t

    val flag :
      ?deprecated:string ->
      ?docv:string -> ?doc:string ->
      ?synonyms:string list ->
      string -> bool t

    val determined : 'a t -> 'a future

    val doc_enum : ?quoted:bool -> (string * 'a) list -> string

    val version : string
    val datadir : string
    val libdir : string
    val confdir : string
  end

  module Context : sig
    type t = ctxt
    type info

    (** [plugins ctxt] enumerates all available plugins.

        If [features] is specified, then enumerates only plugins
        than provide at least one of specified feature.

        If [exclude] is specified, then exclude from the list
        plugins, that has one of the feature specified in the
        [exclude] list.
    *)
    val plugins :
      ?features:string list ->
      ?exclude: string list -> ctxt -> info list


    (** [commands ctxt] enumerates all available commands.

        If [features] and/or [exclude] are specified, then they have
        the same meaning as in {!plugins ~features ~exclude}.*)
    val commands :
      ?features:string list ->
      ?exclude:string list -> ctxt -> info list

    (** [name info] returns the name of a plugin or command. *)
    val name : info -> string

    (** [doc info] returns the short documentation.  *)
    val doc : info -> string


    (** [digest context] returns the [context] digest.

        The digest is a 128-bit MD5 sum of all options of
        all plugins that were selected in the context and match
        the filters specified by the [features] and [exclude]
        parameters.

        See the {!plugins} function for the description of the
        [features] and [exclude] parameters.

        Note: the digest doesn't include the command options and
        arguments only plugins configuration options.
    *)
    val digest :
      ?features:string list ->
      ?exclude:string list -> ctxt -> string

    val features : ctxt -> string list


    (** [get ctxt p] extracts the value of [p] from the context [ctxt] *)
    val get : ctxt -> 'a Parameter.t -> 'a

    (** prints the context  *)
    val pp : Format.formatter -> ctxt -> unit
  end


  module Syntax : sig
    val (-->) : ctxt -> 'a Parameter.t -> 'a
  end

  module Type : sig
    type 'a t = 'a typ

    val define :
      ?digest:('a -> string) ->
      parse:(string -> 'a) ->
      print:('a -> string) -> 'a -> 'a t

    val bool : bool t
    val char : char t
    val int : int t
    val nativeint : nativeint t
    val int32 : int32 t
    val int64 : int64 t
    val float : float t
    val string : string t
    val enum : (string * 'a) list -> 'a t
    val file : string t
    val dir : string t
    val non_dir_file : string t
    val list : ?sep:char -> 'a t -> 'a list t
    val array : ?sep:char -> 'a t -> 'a array t
    val pair : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t
    val t2 : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t
    val t3 : ?sep:char -> 'a t -> 'b t -> 'c t ->
      ('a * 'b * 'c) t
    val t4 : ?sep:char -> 'a t -> 'b t -> 'c t ->
      'd t -> ('a * 'b * 'c * 'd) t
    val some : ?none:string -> 'a t -> 'a option t
  end


  module Error : sig
    type t = error = ..
    type t += Configuration
    type t += Invalid of string
    type t += Exit_requested of int
    type t += Unknown_plugin of string
    type t += Bug of exn * string

    val pp : Format.formatter -> t -> unit
    val register_printer : (t -> string option) -> unit
  end
end
