
(**
   [{v
       LP32	 ILP32	 ILP64	 LLP64	 LP64
 char	  8	    8	     8	     8	    8
 short	 16	   16	    16	    16	   16
 int	 16	   32	    64	    32	   32
 long	 32	   32	    64	    32	   64
 addr    32	   32	    64	    64	   64
 v}]

*)

type model32 = [
  | `LP32
  | `ILP32
]

type model64 = [
  | `ILP64
  | `LLP64
  | `LP64
]

type model = [model32 | model64]
