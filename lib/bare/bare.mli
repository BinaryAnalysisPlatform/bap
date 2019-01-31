open Core_kernel


(** Binary Analysis Rule Engine (BARE).

    The Binary Analysis Rule Engine is a production system (a forward
    chaining rule system) that is used in the Binary Analysis
    Platform, and features non-linear pattern matching and a stream
    based approach to the state of the world.

    BARE defines the syntax and semantics of a rule specification
    language, and provides a simple pattern matching engine. The
    semantics of facts, as well as the semantics of the evaluation
    loop are left to concrete implementations of interpreters.

    A BARE specification consists of a sequence of rules. Each rule is
    a pair. The first element, called the left hand side, is a list of
    tuples that represents patterns that should be matched with
    facts. The second element, called the right hand side, is a list
    of tuples that represents the list of facts that should be
    produced once all patterns on the left hand side match with the
    provided facts. A rule is applied to a stream of facts, if all
    patterns in a rule matches with some facts, then a rule is
    triggered and produces facts from the right hand side. A rule may
    have variables on the left hand side, that will bind to elements
    of a fact tuple that matched with a pattern. The same variable may
    occur in different patterns of the same rule, i.e., a pattern is not
    required to be linear. When the same variable occurs in two
    patterns of the same rule, then a constraint is imposed that this
    variable should be bound to the same term in both patterns. The
    order of the patterns in the left hand side doesn't matter, thus
    the semantics of rule doesn't change with the permutation of its
    patterns. The order of facts may matter, depending on a concrete
    interpretation of fact. If a rule is applied to different
    permutations of a list of facts it may produce different sequences
    of facts, though all derivable facts will be produced.

    BARE represents tuples with S-expressions and variables with atoms
    that start with the question mark (?). A single question mark
    character represents a wild card variable that matches with
    everything but cannot be used on the right hand side of a
    rule. Since the right hand side of a rule may contain more than
    one pattern the engine will perform the join operation on a stream
    of events. Given that any join operation generates a number of
    matches that is in general a polynomial function of the number of
    patterns, a caution should be used by an expert who builds a
    system with complex rules that contains many patterns in the same
    rule. BARE can't apply any query optimization techniques on a
    rule, given the open and extensible notion of a fact. Thus BARE
    should be considered as a base representation for the
    pattern/action forward chanining systems, that shall perform all
    the necessary optimizations on their level.

    {2 Example}

    Suppose we want to process a stream of events that represent the
    behavior of some memory allocator. We expect the following kinds
    of events:

     - [(acquire SITE PTR LEN)] represents an allocation event, where
       SITE is an allocation site (i.e., an address of a program
       instruction that perfroms an allocation), [PTR] is a pointer to
       the allocated data, and [LEN] is the data size;

     - [(release SITE PTR LEN)] represents a memory deallocation
       event;

     - [(violate SITE PTR KIND)] represents a violation of a memory
       allocator invariants of the specified KIND.

     Suppose we want to build a rule, that will report each violation
     as a tuple that contains a kind of the violation and all three
     sites, that are involved in the incident.

    {v
    (((acquire ?asite ?ptr ?)
      (release ?rsite ?ptr ?)
      (violate ?vsite ?ptr ?kind)

     ((?kind ?asite ?rsite ?vsite)))
    v}

    This rule can be read as, for all acquire, release, and violate
    events that have the same [PTR] produce a fact of a violation of
    the specified kind that contains the program locations of these
    three events. For example, given the following sequence of events:

    {v
      (acquire L10 0xBAD 10)
      (release L20 0XBAD 10)
      (violate L30 0XBAD use-after-free)
      (violate L40 0xBAD double-free)
      (violate L50 0XBAD use-after-free)
    v}

    The following facts will be produced if the rule is sequentially
    applied to the above sequence:

    {v
      (use-after-free L10 L20 L30)
      (double-free L10 L20 L40)
      (use-after-free L10 L20 L50)
    v}

    {2 Syntax}

    {3 Concrete Syntax}

    A tuple is represented by an arbitrary S-expression. A variable is
    an atom that starts with the question mark. A special variable [?]
    (one question mark symbol) may occur on the left hand side of a
    rule, and represents a freshly created variable. Every occurence
    of the [?] symbol represents a different variable. Both sides of a
    rule may be empty (represented by the 0-tuple [()]).

    {v
    rule,r  ::= ((p1 .. pM) (f1 .. fN))
    sexp,s  ::= atom | (s1 .. sM)
    patt,p  ::= sexp
    fact,f  ::= sexp
    atom,a  ::= ?sequence-of-chars?
    v}

    Example:
    {v
    (((duck ?name) (does ?action)) ((looks like ?name did ?action)))
    v}

    This rule contains two patterns [(duck ?name)] and [(does ?action)]
    and one production fact [(looks like ?name did ?action)].

    {3 Abstract Syntax}

    A rule r is a tuple (P,F) where P is a sequence of patterns
    p1,...,pM, F is a sequence of facts f1,...,fN, and M,N are
    non-negative numbers. A pattern p and fact f are arbitrary
    n-tuples (terms) t, that contain atoms, other tuples, and free
    variables.

    {v
    rule,r ::= (B,P,F)
    P ::= p1,...
    F ::= f1,...
    B ::= v1->f,...
    V ::= v1,...
    T ::= t1,...
    term,p,f,s,t ::= v | a | (t,..,tM)
    atom,x,y,z ::= ?atom?
    v ::= ?variable?
    v}


    {2 Semantics}

    We use a regular sequent calculus (with all structural rules) to
    represent semantics. A rule with a sequence of patterns P matches
    with a sequence of facts with the given valuations, denoted as
    [B,T |- P], if it can be proved using the rules below (plus
    regular rules of the sequent calculus). The (match-rule) rule,
    expands rules (eliminating tuples on the top level). The
    (match-tuple) rule further expands patterns until each pattern is
    either an atom, in that case it is proved by the axiom of
    identity, or to a variable, in that case a match can be proved with
    the (match-var) rule, that says that a variable matches a
    term only if it is bound to that term.

    Note, that exchange and weakening rules of sequent calculus allows
    us to drop and rearrange facts. The latter makes the order of
    rules irrelevant. If for a sequence of facts, there are several
    valuations of variables that will lead to a match, then all these
    matches are provided. In other words, the engine will generate all
    derivable facts in an unspecified but consistent order.

    {v

     B,T |- p1 .. B,T |- pM
     ----------------------- (match-rule)
     B,T |- (p1,..,pM)


     B,t1,T |- s1 .. B,tM,T |- sM
     ---------------------------- (match-tuple)
     B,(t1,..,tM),T |- (s1,..,sM)


     B,T |- v->t
     ---------- (match-var)
     B,t,T |- v

    v}
*)


(** representation of a tuple  *)
type tuple = Sexp.t

(** representation of a fact  *)
type fact = tuple



(** Matching rule specification.

    A rule is consists of the rule specification and matching state,
    thus applying a rule to a fact will produce another rule that will
    store the partial matching state and, if some states reached the
    completion, it will also produce a sequence of facts.*)
module Rule : sig


  (** [rule abstract type]  *)
  type t [@@deriving sexp_of]



  (** an abstract representation of an error  *)
  type error

  (** {3 Rule Parsing}  *)


  val from_string : string -> (t list, error) Result.t

  (** [from_file name] parses a file that contains zero or more rule
      specifications. Returns [Ok rules] if all rules specifications
      were well-formed, otherwise returns [Error e] with a detailed
      description of an error and a location of a subterm that is not
      part of the grammar.  *)
  val from_file : string -> (t list, error) Result.t

  (** [of_string s] parses the rule specification [s].

      Precondition: [s] is a well-formed rule specification. *)
  val of_string : string -> t


  (** {3 Rule introspecting}  *)


  (** [lhs rule] is the left hand side of the rule.  *)
  val lhs : t -> tuple list


  (** [rhs rule] is the right hand side of the rule  *)
  val rhs : t -> fact list


  (** [spec rule] is the human readable and machine parseable
      well-formed rule specification.  *)
  val spec : t -> string


  (** [pp ppf rule] prints [rule] into the formatter [ppf].  *)
  val pp : Format.formatter -> t -> unit


  (** {3 Rule transformations}  *)

  (** [apply rule fact] applies [rule] to [fact] and produces a new
      rule that contains a partial mathcing state, as well as a list
      (possibly empty) of newly produced facts.  *)
  val apply : t -> fact -> t * fact list

  (** [reset rule] discards the matching state and returns a fresh rule. *)
  val reset : t -> t

  val report_error : ?filename:string -> Format.formatter -> error -> unit
end
