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


    {2 The Command Line Interface}

    The [Bap_main] library provides a few functions that could be used
    to create composable command line interfaces. The final grammar
    specification is build from piecies and is having the following
    EBNF definition:

    {v
    G =
      | "bap", common-options
      | "bap" "<command1>", command1-grammar, common-options
      ..
      | "bap" "<commandN>", commandN-grammar, common-options

    common-options =
      | ""
      | {"-L", [=], string}
      | {"--load-path", [=], string}
      | {"--plugin-path", [=], string}
      | ["--log-dir", [=], string | "--log-dir", [=], string]
      | "--recipe", recipe-grammar
      | "--version"
      | "--help", [[=], help-format]
      | "--help-<plugin1>", [[=], help-format]
      ...
      | "--help-<pluginN>", [[=], help-format]
      | "--<plugin1>"
      ...
      | "--<pluginN>"
      | "--no-<plugin1>"
      ...
      | "--no-<pluginN>"
      | plugin1-grammar
      ...
      | pluginN-grammar
  v}


    Each command can define its own syntax and use the full power of
    the command line (including positional arguments and short keys)
    as long as it doesn't introduce conflicts with the
    [common-options] grammar.

    The [common-options] grammar defines the syntax that is used to
    specify plugin configuration parameters. Each plugin can register
    its own parameters, but in a restricted way, e.g., no positional
    arguments, all parameter names must be long and will be
    automatically prefixed with the plugin name. Plugins configuration
    parameters form the configuration context for each invocation of
    BAP. These parameters also do not need an access to the command
    line, and could be specified via configuration files, environment,
    etc.

    A couple of predefined rules are added to the [common-options]
    grammar. First of all, for each registered [<plugin>]  the
    ["--no-<plugin>"] option is added, which if specified, will
    disable the plugin. A disabled plugin will still contribute to
    the command line grammar, but the extensions which are registred
    with it will not be loaded. Unless the extension is the command
    itself, which will be still evaluated if selected on the command
    line.

    Also, for each registred [<plugin>] an option [--<plugin>] will be
    added to enable the backward compatibility with the old style of
    specifying passes.

    Another rule which is added on per plugin basis, is the
    [--help-<plugin>] rule which will render a manual page for the
    given [<plugin>].

    The [-L] and [--logdir] options are preparsed on the command line
    and are used to specify the plugins search path (which obviously
    should be specified before we can load plugins) and the logging
    destination which we would like to know as soon as possible.


    The [--recipe] option is very special, as it changes the command
    line itself. Every occurence of the [--recipe] option will parse
    the provided recipe, which will be evaluated to the list of
    arguments which will be substituted instead of the specified
    [--recipe] option. See [bap recipes] for more information about
    the recipes.

    Finally, the common [--version] and [--help] options are added
    with an expected semantics.

    For the detailed description of the command line interface read
    the manual page generated with [bap --help].

    Note, the actual parser is less strict than the grammar and may
    accepts inputs that are not recognized by the grammar.
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

    (** description of the command line syntax.

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

    (** [declare grammar name command] declares a [command].

        Declares to BAP that a command with the given [name] should be
        invoked when a user specifies the [name] or [:name] as the
        first argument in the command line. The [grammar] defines
        the command line grammar of this command.


        where [<command1>] ... [<commandN>] are the names of declared
        commands, [grammar1] ... [grammarN] are corresponding grammars
        for each command, and [G'] is the global

        When the command is selected and command line arguments are
        parsed succesfully, the [command] function is applied to the
        the specified command line arguments. The result of evaluation
        of the [command] will become the result of the [Bap_main.init
        ()] expression in the host program.

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


    (** [args] is the empty grammar.
        Useful to define commands that do not take arguments or
        as the initial grammar which is later extended with parameters
        using the [$] operator (see below).
    *)
    val args : ('a, 'a) t


    (** [args t $ t'] extends the grammar specification [t] with [t'].*)
    val ($) : ('a,'b -> 'c) t -> 'b param -> ('a,'c) t


    (** [argument t] declares a positional argument of type [t].

        The grammar of [args $ term $ argument t]:
        {v
          G = term, [t] | [t], term
        v}

    *)
    val argument :
      ?docv:string ->
      ?doc:string ->
      'a typ -> 'a param


    (** [arguments t] declares an infinite number of positional
        arguments of type [t].

        The grammar of [args $ term $ arguments t]:
        {v
          G = term, {t} | {t}, term
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


    (** [switch values name] declares a switch-type parameter.

        The grammar of {args $ term $ switch values name}:
        {v
          G  = term, G' | G', term
          G' = ["--<name v0>" | .. | "--<name vN>"]
        v}

        where [<name vK>] is the result of application of the [name]
        function to the [K]th element of the [values] list.

        The switch-type parameters enables a selection from a list of
        choices. If [--<name vK>] is specified on the command line,
        then [Some vK] will be passed to the command, otherwise,
        the [None] value will be passed.

        The [name] function could be non-injective, so that several
        names can correspond to the same value in the choice list.

        The [name] function shall return syntactically valid command
        line keys, i.e., non-empty strings that do not contain
        whitespaces.
    *)
    val switch :
      ?doc:('a -> string) ->
      'a list ->
      ('a -> string) ->
      'a option param


    (** [switches values name] is multiple choice switch-type parameter.

        The grammar of [args $ term $ switches values name] is
        {v
          G  = term, G' | G', term
          G' = {"--<name v0>" | .. | "--<name vN>"}
        v}

        where [v1] .. [vN] are elements of [values], and [<name vK>]
        is the result of application of the [name] function to the
        [K]th element of the [values] list.

        The switch-type parameters enables a selection from a list of
        choices, however unlike it [switch] counterpart, the selector
        can occur more than once on the command line.
        For every occurence [--<name vK>] on the command line, the
        corresponding [vK] value will be added to the list that will
        be passed as an argument to the command. The order of elements
        in the list will match with the order of selectors on the
        command line.

        The [name] function could be non-injective, so that several
        names can correspond to the same value in the choice list.

        The [name] function shall return syntactically valid command
        line keys, i.e., non-empty strings that do not contain
        whitespaces.
    *)
    val switches :
      ?doc:('a -> string) ->
      'a list ->
      ('a -> string) ->
      'a list param

    (** [dictionary keys t name] declares a dictionary-style parameter.

        The grammar of [args $ term $ dictionary keys t name] is
        {v
          G  = term, G' | G', term
          G' = ["--<name k0>" [=] t] | .. | ["--<name kN>" [=] t]
        v}

        where [k0] .. [kN] are elements of the [keys] list and [<name
        kN>] is the result of application of the [name] function to
        the [kN] element of the [keys] list.

        For each occurence of [--<name k> [=] v] on the specified
        command line a binding [(k,v)] will be added to the
        dictionary, which is passed as an argument to the command.

        Each key-value pair can occur at most once on the command
        line. If a key-value pair is omitted, then the [default t]
        will be added to the dictionary for that key. The length of the
        passed dictionary is the same as the length of the [keys] list.


        The [name] function could be non-injective, so that several
        names can correspond to the same value in the choice list.

        The [name] function shall return syntactically valid command
        line keys, i.e., non-empty strings that do not contain
        whitespaces.

        {3 Keys as flags}

        When the [as_flag] option is specified makes the value part
        of the grammar becomes optional, thus the grammar of
        [args $ term $ dictionary ~as_flag:s keys t name] becomes

        {v
          G  = term, G' | G', term
          G' = ["--<name k0>" [[=] t]] | .. | ["--<name kN>" [[=] t]]
        v}

        If the value is omitted on the command line, by the key [k] is
        specified, then the [(k,s)] will be added to the dictionary.

        @parameter as_flag enables the "Keys as flags" mode.

        @parameter docv is the name that will be used to reference
        values in the documentation string.

        @parameter doc if specified then [doc k] will be the
        documentation string for the [--<key k>] parameter.
    *)
    val dictionary :
      ?docv:string ->
      ?doc:('k -> string) ->
      ?as_flag:('k -> 'd) ->
      'k list ->
      'd typ ->
      ('k -> string) ->
      ('k * 'd) list param

    (** [parameter t name] declares a generic command line parameter.

        The grammar of [args $ term $ parameter t name]
        {v
          G  = term, G' | G', term
          G' = ["--<name>" [=] t]
        v}

        If [--<name>=v] is specified, then [v] will be passed to the
        command, otherwise the [default t] value will be passed.

        {3 Parameters as flags}

        When the [as_flag] option specified, then the value part of
        becomes optional and the parameter could be specified without
        it, as a flag, e.g., [--<name>], in that case the value passed
        to the [as_flag] parameter will be passed as an argument to
        the command.

        {3 Short keys}

        The [aliases] parameter may additionally contain
        one-character-long names, which will be interpreted as short
        keys, that should be specified with only one dash character,
        i.e.,

        The grammar of [args $ term $ parameter ~aliases:["<k>"] t name]
        {v
          G  = term, G' | G', term
          G' = ["--<name>" [=] t | "-<k>" [=] t]
        v}

        where [<k>] is a single character.

        @parameter as_flag enables the "Parameters as flags" mode.

        @parameter doc is the documentation string.

        @parameter docv is the name used to reference the parameter
        value in its documentation.

        @parameter aliases is a list of additional aliases of the
        parameter.

    *)
    val parameter :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?aliases:string list ->
      'a typ ->
      string -> 'a param



    (** [parameters] declares a generic command line parameter.

        The grammar of [args $ term $ parameters t name]
        {v
          G  = term, G' | G', term
          G' = {"--<name>" [=] t}
        v}

        This command line parameter behaves the same as its [parameter]
        counterpart, but it could be specified more than once on the
        command line. For each occurence of [--<name>=v], [v] will be
        added to the list (in the order of occurence), which will be
        passed as an argument to the command.

        See the {!parameter} function for more details.
    *)
    val parameters :
      ?docv:string ->
      ?doc:string ->
      ?as_flag:'a ->
      ?aliases:string list ->
      string ->
      'a typ -> 'a list param


    (** [flag name] declares a flag-style parameter.

        The flag-style parameter is like a normal [parameter], except
        that it is not possible to specify its value.

        The grammar of [args $ term $ flag name] is

        {v
          G  = term, G' | G', term
          G' = ["--<name>"]
        v}

        The flag could be specified at most once on the command line,
        and if specified then the [true] value will be passed to the
        command.

        The rest of parameters of the [flag] function have the same
        meaning as described in the {!parameter} function.
    *)
    val flag :
      ?docv:string ->
      ?doc:string ->
      ?aliases:string list ->
      string ->
      bool param



    (** [flags] declares a muti-occuring flag-style parameter.

        The grammar of [args $ term $ flag name] is

        {v
          G  = term, G' | G', term
          G' = {"--<name>"}
        v}

        Unlike it {!flag} counterparts parameters declared as [flags]
        make occur more than once on the command line. The number of
        occurences will be passed to the command.
    *)
    val flags :
      ?docv:string ->
      ?doc:string ->
      ?aliases:string list ->
      string ->
      int param
  end


  (** Configuration Parameters.

      Use this module to declare and use configuration parameters for
      your plugins. Algthough configuration parameters could be
      specified via the command line, they are different from the
      corresponding parameters of commands in several ways:

      - configuration parameters could be still passed even when there
        is no command line interface (via configuration files and
        environment);

      - they are specific to plugins and are always prefixed with the
        plugin name.
  *)
  module Configuration : sig

    (** a configuration parameter  *)
    type 'a param


    (** the current configuration.  *)
    type t = ctxt

    (** a piece of information about a system component. *)
    type info


    (** [get ctxt parameter] gets the value of the [parameter].

        Accesses the value of the previously defined [parameter].

        The [Extension.Syntax] module also provides an infix version
        of this function under the [-->] name.
    *)
    val get : ctxt -> 'a param -> 'a


    (** [parameter t name] declares a configuration parameter.

        This declaration extends the [common-options] grammar by
        adding the following rules
        {v
          common-options =
            ...
            | common-options, R | R, common-options
          R = ["--<plugin>-<name>", ["="], t]
        v},

        where [<plugin>] is the name of the plugin in which the
        configuration parameter is specified. (Note, the name of a
        plugin is the name of the file in which it is packed without
        the extension, e.g., a plugin [foo.plugin] has name [foo]).

        When the [--<plugin>-<name> v] is specified on the command
        line, or a configuration file, or in the environment, then
        [get ctxt p] will evaluate to [v], where [p] is the declared
        parameter.

        The [as_flag] option makes the value part optional on the
        command line, so that [declare ~as_flag=v t name] extends
        the grammar by adding the following rules
        {v
          common-options =
            ...
            | common-options, R | R, common-options
          R = ["--<plugin>-<name>", [["="], t]]
        v},

        Then, if the parameter was specified on the command line
        without an argument, then [v] will be used as the value of the
        parameter.

        When [aliases] are specified, then for each [name] in the
        aliases a [--<plugin>-<name>] option will be added.

        Note, even if the name is short (i.e., consisting only of one
        letter) it will still be prefixed with the plugin name and
        interpreted as a long option, e.g., if name is ["k"] and the
        plugin name is ["foo"], then the option name will be
        ["--foo-k"].

        {Examples}

        Declaring a simple configuration parameter:

        {[
          open Core_kernel
          open Bap_main.Extension

          let depth = Configuration.parameter Type.int "depth"

          let () =
            Bap_main.Extension.declare @@ fun ctxt ->
            printf "Will dive to depth %d\n"
              (Configuration.get ctxt depth)
        ]}

        The [Extension.Syntax] module adds an infix [(-->)] operator
        for the [get] function. Using this operator the previous
        example could be rewritten as:

        {[
          open Core_kernel
          open Bap_main.Extension
          open Bap_main.Extension.Syntax

          let depth = Configuration.parameter Type.int "depth"

          let () =
            Bap_main.Extension.declare @@ fun ctxt ->
            printf "Will dive to depth %d\n" (ctxt-->depth)
        ]}
    *)
    val parameter :
      ?docv:string -> ?doc:string ->
      ?as_flag:'a ->
      ?aliases:string list ->
      'a typ -> string -> 'a param


    (** [parameters t name] declares a multi-occuring parameter.

        This declaration extends the [common-options] grammar by
        adding the following rules
        {v
          common-options =
            ...
            | common-options, R | R, common-options
          R = {"--<plugin>-<name>", ["="], t}
        v},

        where [<plugin>] is the name of the plugin in which the
        configuration parameter is specified. (Note, the name of a
        plugin is the name of the file in which it is packed without
        the extension, e.g., a plugin [foo.plugin] has name [foo]).

        Every time the [--<plugin>-<name> v] is specified on the
        command line, or a configuration file, or in the environment,
        then [v] is added to the list to which [get ctxt p], where [p]
        is the declared parameter.

        The rest of the parameters have the same meaning as in
        the {!parameter} function.
    *)
    val parameters :
      ?docv:string -> ?doc:string ->
      ?as_flag:'a ->
      ?aliases:string list ->
      'a typ -> string -> 'a list param


    (** [flag name] declares a parameter that can be used as a flag.

        This is a specialization of a more general {!parameter}
        function. The [common-options] grammar is extended with the
        following rules:

        {v
          common-options =
            ...
            | common-options, R | R, common-options
          R = ["--<plugin>-<name>"]
        v},

        where [<plugin>] is the name of the plugin in which the
        configuration parameter is specified. (Note, the name of a
        plugin is the name of the file in which it is packed without
        the extension, e.g., a plugin [foo.plugin] has name [foo]).

        When the [--<plugin>-<name> v] is specified on the command
        line, or in the environment, then [get ctxt p] will evaluate
        to [true], where [p] is the declared parameter.
    *)
    val flag :
      ?docv:string -> ?doc:string ->
      ?aliases:string list ->
      string -> bool param


    (** [determined p] is a future that becomes determined when
        context is ready.  *)
    val determined : 'a param -> 'a future


    (** [get ctxt version] is the application version.
        This would be the value that was passes to the [version]
        parameter of the {!Bap_main.init} function. *)
    val version : string param


    (** [get ctxt datadir] a directory for BAP readonly data. *)
    val datadir : string param


    (** [get ctxt libdir] a directory for BAP object files,
        libraries, and internal binaries that are not intended to be
        executed directly.  *)
    val libdir : string param


    (** [get ctxt confdir] a directory for BAP specific configuration files *)
    val confdir : string param

    (** [plugins ctxt] enumerates all enabled plugins.

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
    val info_name : info -> string

    (** [doc info] returns the short documentation.  *)
    val info_doc : info -> string

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

    (** prints the context  *)
    val pp_ctxt : Format.formatter -> ctxt -> unit
  end


  (** A lightweight syntax for accessing configuration parameters.

      Once this module is opened it is possible to access the
      parameter value using the infix notation, e.g., [ctxt-->arch].
  *)
  module Syntax : sig
    val (-->) : ctxt -> 'a Configuration.param -> 'a
  end


  (** Data types for parameters.

  *)
  module Type : sig
    type 'a t = 'a typ


    (** [define ~parse ~print default] defines a data type.

        The [print x] is the textual representation of the value
        [x]. For all [x] in the defined type, [x = parse (print x)].

        The [digest x] function, if provided, should return a compact
        representation of [x], which will be used for computing the
        digest of the parameter. This option is useful for parameters
        which a references by themselves (e.g., files, directories).
    *)
    val define :
      ?digest:('a -> string) ->
      parse:(string -> 'a) ->
      print:('a -> string) -> 'a -> 'a t


    (** [t || x] defines a new type with different default.

        The new type has the same definition as [t] except the default
        value is [x].
    *)
    val (||) : 'a t -> 'a -> 'a t


    (** [bool] is ["true" | "false"]  *)
    val bool : bool t


    (** [char] is a single character.  *)
    val char : char t


    (** [int] is a sequence of digits.

        Common OCaml syntax is supported, with binary, decimal,
        and hexadecimal literals.
    *)
    val int : int t


    (** [nativeint] is a sequence of digit.

        This type uses processor-native integer as OCaml representation so it
        is one bit wider than the [int] type.
    *)
    val nativeint : nativeint t


    (** [int32] is a sequence of digits. *)
    val int32 : int32 t

    (** [int64] is a sequence of digits. *)
    val int64 : int64 t

    (** [float] is a floating point number. *)
    val float : float t


    (** [string] is a sequence of bytes.

        When the sequence contains whitespaces, delimit the whole
        sequence with double or single quotes.*)
    val string : string t


    (** [some t] extends [t] with an empty string. *)
    val some : 'a t -> 'a option t


    (** [enum repr] defines a type from the given representation.

        Defines a type with such [print] and [parse], that for each
        pair [(s,v)] in [repr], [print v = s] and [parse s = v].

        It is a configuration error, when [repr] is empty.

        If [repr] has repretitive keys, i.e., for the same textual
        representation there are different values, then the result is
        undefined.
    *)
    val enum : (string * 'a) list -> 'a t


    (** [file] is the file or directory name.

        The file denoted by the name must exist.

        If the name references to a regular file then the [digest] of the
        file is the digest of its contents and the name itself doesn't
        affect the digest value.

        Otherwise, if the name doesn't refer to a regular file, then
        the digest will depend on the name itself and the last
        modification time of the file.
    *)

    val file : string t


    (** [dir] denotes a file which must be a directory.

        The directory denoted by the name must exist. See the [file]
        type for more information about computing the digest.
    *)
    val dir : string t

    (** [dir] denotes a file which must not be a directory.

        The directory denoted by the name must exist. See the [file]
        type for more information about computing the digest.
    *)
    val non_dir_file : string t


    (** [list ~sep t] is a list of [t] elements, separated with [sep].   *)
    val list : ?sep:char -> 'a t -> 'a list t


    (** [array ~sep t] is an array of [t] elements, separated with [sep].
        @parameter sep defaults to [','].
    *)
    val array : ?sep:char -> 'a t -> 'a array t

    (** [pair ~sep t1 t2] is a pair [t1] and [t2], separated with [sep].

        @parameter sep defaults to [',']. *)
    val pair : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t

    (** [t2 ~sep t1 t2] is a pair [t1] and [t2], separated with [sep].

        @parameter sep defaults to [','].
    *)
    val t2 : ?sep:char -> 'a t -> 'b t -> ('a * 'b) t


    (** [t3 ~sep t1 t2 t3] is ([t1],[t2],[t3]), separated with [sep].
        @parameter sep defaults to [',']. *)
    val t3 : ?sep:char -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    (** [t4 ~sep t1 t2 t3 t4] is ([t1],[t2],[t3],[t4), separated with [sep].
        @parameter sep defaults to [',']. *)
    val t4 : ?sep:char -> 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  end


  (** An extensible set of possible errors  *)
  module Error : sig
    type t = error = ..


    type t += Configuration

    type t += Invalid of string

    type t += Exit_requested of int

    type t += Unknown_plugin of string

    type t += Bug of exn * string


    (** [pp ppf err] outputs a human readable description of [err]  *)
    val pp : Format.formatter -> t -> unit

    (** [register_printer to_string] registers a printer for a subset
        of the errors. *)
    val register_printer : (t -> string option) -> unit
  end
end
