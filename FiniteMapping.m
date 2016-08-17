(* TODO *)
(*
Add messages/checks for various error conditions, 
including
- Repeated keys
- make injective a special case (head)
- allow empty? yes

- support dense and sparse arrays with linear offset and negative indices for GridData

- Consider overloading Listable operations to this (e.g. Plus, Times, Limit).
- Consider supporting maps of the form A -> B -> C -> D (right associative) as a first-class feature
 Note that B stays the same for all a in A, this is not enforced by an A -> FiniteMapping

Add support for typechecking on LHS/RHS.

Allow computational/lazy expression of data and functions
 iterators
*)

(* ^^ End ^^ *)

BeginPackage["FiniteMapping`", {"paul`", "PackageDeveloper`"}]

ClearAll["FiniteMapping`*", "FiniteMapping`Private`*"]


PublicSymbols[
  FiniteMappingQ
,FiniteMappingMakeConstant
,FMMakeListDomainNames
,FiniteMappingMakeFromList
,FMForgetDomain
,FiniteMappingMakeFromLists
,FiniteMappingMakeFromArray
,FiniteMappingMakeFromExpression
,FiniteMappingMakeFromRules
,FMDomain
,FMAsRules
,FMAsAssociation
,FMEvaluate
,FMEvaluateIndexed
,FMEvaluateMultiple
,FMEvaluateIndexedMultiple
,FMEvaluateAll
,FMInjectiveQ
,FMLength
,FMInverse
,FMGeneralizedInverse
,FMCompose
,FMMap
,FMMapKeyed
,FMMapValues
,FMMapDomain
,FMConcat
,FMUpdate
,FMDomainSubset
,FMDeleteOne
,FMDeleteMultiple
,FMRuleCases
,FMSelect
,FMCases
,FMDeleteCases
,FMDomainSelect
,FMDomainCases
,FMDomainDeleteCases
,FMPairing
,FMValuesMatrix
,FMDomainMatrix
,FMUniverse
,FMRange
,FMUniverseRange

,FiniteMapping
]

Begin["`Private`"]

DefinePublicFunction[
FMUniverseRange[f_FiniteMapping],"gives universe and range",Through[{FMUniverse, FMRange}@f]];


DefinePublicFunction[
  FMUniverse[f_FiniteMapping]
  ,"most FM don't have these specified"
  ,_
];


DefinePublicFunction[
  FMRange[f_FiniteMapping]
  ,"most FM don't have these specified"
  ,_
];


FMUtilGuessPattern[elements_List] (* TODO*)

(* whether everything that matches p1 also matches p2 *)
(* heuristic only *)
FMUtilPatternSubset[p1_, p2_]  (* TODO*)

DefinePublicFunction[
FiniteMappingQ[_FiniteMapping] ,"whether this is a fm", True];
DefinePublicFunction[
FiniteMappingQ[_] ,"most things are not", False];
(*
(* TODO *)
FiniteMapping /: Unequal[]
    *)
(* -- Purpose -- *)
(*
Stores the information associated with a finite mapping.
*)

(* Implementation Notes *)
(*
Valid forms:

Basic storage:

FiniteMapping[Rules, Association]
  Efficient for named lookup

FiniteMapping[Lists, vars_List, vals_List | SparseArray, varsPositionFunction_]
   varsPositionFunction caches PositionFunction[vars]
   Efficient for indexed lookup while still having a general, named variable set.

FiniteMapping[Expression, levelspec, expression] 
    maps (a subset of) valid Positions in an expression/Packed Array to the corresponding subexpressions
    this can be used with PackedArray type expressions, dense matrices represented as lists of lists etc.
    Also works with SparseArrays (todo: check all operations)

    Prefer the next form when the array is not ragged.

    TODO hold the expression completely to avoid it ever being re-evaluated

FiniteMapping[Array, level, array]
    IsArrayAtLevel[array, level] === True
    Such dense or sparse initially non-ragged arrays can be efficiently read by (flat) index (using FlatIndexToPosition)

    maps valid Positions on a given level in an expression/Packed Array to the corresponding subexpressions
    this can be used with PackedArray type expressions, dense matrices represented as lists of lists etc.
    Also works with SparseArrays (todo: check all operations)

TODO note that SparseArray can store non-numeric data
TODO clean up comments
TODO argument checks, make optional?
 logging, make optional (debug build..)

FiniteMapping[SharedDomains, domains:{{___}..}, data_ /; data_?ArrayAtLevelQ[Length@domains]
    Stores a nested finite mappings that all have the same domain:
     f: A1 ->  (A2 ->  (...->  (An -> C)...)
    suitable for dense/sparse arrays of any dimension and datasets with rows and columns.
    Such finite mappings support very efficient lookups of Data

FiniteMapping[Dataset, domains:{{___}..}, data_ /; data_?ArrayAtLevelQ[Length@domains]
    Stores a nested finite mappings that all have the same domain:
     f: A1 ->  (A2 ->  (...->  (An -> C)...)
    suitable for dense/sparse arrays of any dimension and datasets with rows and columns.
    Such finite mappings support very efficient lookups of Data

Wrappers

FiniteMapping[UniverseRange, f_FiniteMapping, universe_, range_]
    Stores a FiniteMapping together with explicit universe and range.

    Speeds up FMRangeSubsetQ and similar queries.


FiniteMapping[InjectiveQ, f_FiniteMapping, injectiveQ_Boolean]
    Stores a FiniteMapping together with known injectivity status.

    Speeds up FMInjectiveQ and similar queries.


Not implemented:

FiniteMapping[String, <data>] (* maps indices to letters *)
FiniteMapping[Strings, <data>] (* list of strings, packed into one String internally to save storage (is this worth it?) *)
(* other reindexing mappings (e.g. for treating a packedArray as ragged), compositions, ..., renamings *)


*)

(* Name *)

FiniteMapping

(* Attributes *)

FiniteMapping~SetAttributes~HoldAllComplete

(* ^^ End ^^ *)



DefinePublicFunction[
FiniteMappingMakeFromRules[ruleset : _[_[_,_]...]]
  ,"Creates a new finite mapping from a list of \"rules\"

List of rules can be provided any 'list' of pairs (a,b).
Patterns in left-hand sides are treated as Verbatim"

,With[{a = Association@Evaluate[List@@Rule@@@ruleset]},
    FiniteMapping[Rules, a]
]
  ];

(* ^^ End ^^ *)



(* -- Purpose -- *)


DefinePublicFunction[
  FiniteMappingMakeConstant[vars_List, val_]
  ,"Creates a new finite mapping from parts and position specifications of an expression."
  ,FiniteMappingMakeFromLists[vars, SparseArray[{}, Length@vars, val]]
  ];


DefinePublicFunction[
FMMakeListDomainNames[length_Integer]
  ," this will be the FMDomain of a list of length
This can be considered a default naming scheme, used in case the user does not care."
  ,  Array[List, length]
];

DefinePublicFunction[
FiniteMappingMakeFromList[l_List]
 , "same as from a 1-d array"
  ,FiniteMappingMakeFromArray[l, 1]
];

DefinePublicFunction[
  FMForgetDomain[f_FiniteMapping]
  ,"drop all input names: treat as a plain list from now on"
  ,FiniteMappingMakeFromList@FMEvaluateAll@f
  ];

(*
DefinePublicFunction[
FMForgetDomain[f : FiniteMapping[Array | Expression, ___]] := f; (* TODO this is a noop anyhow - bug if called? On the other hand, this is not the right implementation: the domain will still be multi-dimensional after this *)
*)

DefinePublicFunction[
FiniteMappingMakeFromLists[a_List, fa : _List | _SparseArray?VectorQ] /; Length@a == Length@fa && DuplicateFreeQ@a
  ,"Keys and Values passed separately"
  ,  With[{vpf = PositionFunction@a},
  FiniteMapping[Lists, a, fa, vpf]
  ]
];

DefinePublicFunction[
FiniteMappingMakeFromArray[array_?ArrayQ]
  ,"from an n-d dense array"
  ,FiniteMappingMakeFromArray[array, ArrayDepth@array]
  ];


DefinePublicFunction[
FiniteMappingMakeFromArray[array_, level_Integer /; level > 0] /; IsArrayAtLevel[array, level]
  ,"at a given level"
  ,FiniteMapping[Array, level, array]
  ];

(* TODO support Heads->True *)

DefinePublicFunction[
FiniteMappingMakeFromExpression[e_, levelspec_]
  ,"indexed by Positions"
  ,FiniteMapping[Expression, levelspec, e]
  ];

(* ^^ End ^^ *)



DefinePublicFunction[
FMDomain[f : FiniteMapping[Rules, assoc_Association]]
 , "A

Similar to Keys@Association"
  ,Keys@assoc
];

DefinePublicFunction[FMDomain[f : FiniteMapping[Lists, a_, fa_, vpf_]],"", a];
DefinePublicFunction[FMDomain[f : FiniteMapping[Expression, levelspec_, e_]] ,"", PositionsOnLevel[e, levelspec]];
DefinePublicFunction[FMDomain[f : FiniteMapping[Array, level_, array_]] ,"", Array[List@##&,Dimensions[array][[1;;level]]]~Flatten~(level-1)];

DefinePublicFunction[FMAsRules[f : FiniteMapping[Rules, assoc_Association]] ,"As a list of rules", Normal@assoc];
(*FMAsRules[f : FiniteMapping[Array, level_, array_]] /; TensorRank@array == level := ArrayRules@array (* TODO only works for level === TensorRank , adds a default rule, does not include 0*)*)
DefinePublicFunction[FMAsRules[f_FiniteMapping] ,"", Thread[Rule[FMDomain@f, FMEvaluateAll[f]]]];

DefinePublicFunction[FMAsAssociation[f : FiniteMapping[Rules, assoc_Association]] ,"", assoc];
DefinePublicFunction[FMAsAssociation[f_FiniteMapping] ,"", Association@FMAsRules@f];

(* -- Purpose -- *)
(*
As an expression when A is a (sparse) list of position indices
Fill missing values with whatever is provided, use List or user specified head where unavailable.

TODO
*)


DefinePublicFunction[FMEvaluate[f : FiniteMapping[Rules, assoc_Association], x_] ,"f(x)", assoc~Lookup~Key@x];
DefinePublicFunction[FMEvaluate[f : FiniteMapping[Lists, a_, fa_, vpf_], x_] ,"f(x)", fa~Extract~vpf@x];
DefinePublicFunction[FMEvaluate[f : FiniteMapping[Expression, levelspec_, e_], x_] ,"f(x)", e~Extract~x];
DefinePublicFunction[FMEvaluate[f : FiniteMapping[Array, level_, array_], x_] ,"f(x)", array~Extract~x];

DefinePublicFunction[FMEvaluateIndexed[f : FiniteMapping[Rules, assoc_Association], x_Integer]
  ,"f(x) where x is specified via an index into A"
  ,assoc[[x]]
];

DefinePublicFunction[
  FMEvaluateIndexed[f : FiniteMapping[Array, level_, array_], x_Integer]
  ,""
  , array~Extract~FlatIndexToPosition[x,Dimensions[array][[1;;level]]]
];

DefinePublicFunction[FMEvaluateIndexed[f_FiniteMapping, x_Integer]  ,"", FMEvaluateAll[f][[x]]]; (* TODO can we do better for Expression? At least keep stuff held... *)


DefinePublicFunction[
  FMEvaluateMultiple[f : FiniteMapping[Rules, assoc_Association], xs_List]
  ,"f(A')

in a list of the same order"
  ,assoc~Lookup~xs
];

DefinePublicFunction[FMEvaluateMultiple[f : FiniteMapping[Lists, a_, fa_, vpf_], xs_List],"", fa~Extract~(List@*vpf/@xs)]; (* Note: must wrap a list around single integer position specification *)
DefinePublicFunction[FMEvaluateMultiple[f : FiniteMapping[Expression, levelspec_, e_], xs_List] ,"", e~Extract~xs];
DefinePublicFunction[FMEvaluateMultiple[f : FiniteMapping[Array, level_, array_], xs_List] ,"", array~Extract~xs];


DefinePublicFunction[
  FMEvaluateIndexedMultiple[f : FiniteMapping[Rules, assoc_Association], xs : {___Integer}]
  ,"f(A') where A' is specified as an index subset"
  ,Values@assoc[[xs]]
];

DefinePublicFunction[
FMEvaluateIndexedMultiple[f : FiniteMapping[Lists, a_, fa_, vpf_], xs : {___Integer}]
  ,""
  , fa[[xs]]
];

DefinePublicFunction[
FMEvaluateIndexedMultiple[f_FiniteMapping, xs_List]
  ,""
  ,FMEvaluateIndexed[f, #] & /@ xs

];


DefinePublicFunction[
FMEvaluateAll[f : FiniteMapping[Rules, assoc_Association]] 
  ,"f(A)

in a list of the same order as FMDomain@f

This is similar to Values@Association"
  ,Values@assoc
];

DefinePublicFunction[FMEvaluateAll[f : FiniteMapping[Lists, a_, fa_, vpf_]] ,"", fa];
DefinePublicFunction[FMEvaluateAll[f : FiniteMapping[Array, level_, array_]] ,"", Flatten[array, level-1]];
DefinePublicFunction[FMEvaluateAll[f_FiniteMapping] ,"", FMEvaluateMultiple[f, FMDomain@f]];


DefinePublicFunction[FMInjectiveQ[f_FiniteMapping]
  ,"Whether f is injective (bijective)"
  ,Length@FMDomain@f === Length@DeleteDuplicates@FMEvaluateAll[f]
];


DefinePublicFunction[
FMLength[f : FiniteMapping[Array, level_, array_]]
  ,"|A|"
  ,Times@@Dimensions[array][[1;;level]]
  ];


DefinePublicFunction[FMLength[f_FiniteMapping],"", Length@FMDomain@f];

DefinePublicFunction[
FMInverse[f_FiniteMapping?FMInjectiveQ]
,"f^-1 for injective f"
, FiniteMappingMakeFromRules@Thread@Rule[
FMEvaluateAll@f, FMDomain@f
]
];


DefinePublicFunction[
FMGeneralizedInverse[f : FiniteMapping[Rules, assoc_Association]]
  ,"Returns g such that g(y) is a list with
x in g(y) iff f(x) = y

This can be computed for any f."

  ,With[{a=
    Association@Evaluate@PositionIndex@assoc},
 FiniteMapping[Rules, a]
]
  ];

DefinePublicFunction[
FMGeneralizedInverse[f_FiniteMapping], "", FMGeneralizedInverse@FiniteMappingMakeFromRules@FMAsRules@f
];


DefinePublicFunction[
FMCompose[f_FiniteMapping, g_FiniteMapping]
  ,"f°g"
  ,  With[{a = FMDomain@g
  , fga = f~FMEvaluate~(g~FMEvaluate~#) & /@ FMDomain@g
  },
  FiniteMappingMakeFromLists[a,fga]
]
  ];


DefinePublicFunction[
FMMap[f_, e_List]
  ,"
Like Map, but the result is a FiniteMapping instead of a List, indexed by list indices."
  ,FiniteMappingMakeFromList@Map[f, e]
  ]


DefinePublicFunction[
FMMapKeyed[f_, e_]
  ,"Like AssociationMap, but the result is a FiniteMapping"
  ,With[{a = AssociationMap[f, e]},
  FiniteMapping[Rules, a]
]
  ];

(* TODO can do much better in most cases *)
DefinePublicFunction[
FMMapValues[h_, f: FiniteMapping[Rules, assoc_Association]]
  ,"Apply h to each value"
  ,{hassoc = h /@ assoc}~With~FiniteMapping[Rules, hassoc]
  ];

DefinePublicFunction[
FMMapValues[h_, f: FiniteMapping[Lists, a_, fa_, vpf_]]
  ,"",{hfa = h /@ fa}~With~FiniteMapping[Lists, a, hfa, vpf]
  ];

DefinePublicFunction[FMMapValues[h_, f: FiniteMapping[Array, level_, array_]]
  ,"",{harray = Map[h, array, {level}]}~With~FiniteMapping[Array, level, harray]
  ];

DefinePublicFunction[
  FMMapValues[h_, f_FiniteMapping]
  ,"",
  FiniteMappingMakeFromLists[FMDomain@f, h /@ FMEvaluateAll@f]
  ];

DefinePublicFunction[
FMMapDomain[f_, g_FiniteMapping]
  ,"Apply f to each A to produce A'"
  ,FiniteMappingMakeFromLists[f /@ FMDomain@g, FMEvaluateAll@g]
]; (* TODO can do better when keys are explicitly stored (Rules, Lists)*)


DefinePublicFunction[
FMConcat[l : {__FiniteMapping}]
  ,"Produce a finiteMapping with the values of f followed by those of g.
Assumes f and g have disjoint variable names.
Use e.g. FMMapDomain to ensure this"
  
  , FiniteMappingMakeFromLists[
      Join@@(FMDomain /@ l),
      Join@@(FMEvaluateAll /@ l)
    ]
];

(* TODO can do better when keys are explicitly stored (Rules, Lists)*)

DefinePublicFunction[
FMUpdate[f_FiniteMapping, g_FiniteMapping] 
  ,"Update or add certain values.

Prefers values in g"
  ,FiniteMappingMakeFromRules[FMAsRules@f~UpdateRuleList~FMAsRules@g]
  ];


DefinePublicFunction[
FMDomainSubset[f_FiniteMapping, y_List] 
  ,"Keep only values associated with certain a in A."
  ,
  FMDomainSelect[f, MemberQ[y,#]&]
];

DefinePublicFunction[
FMDeleteOne[f_FiniteMapping, y_] 
  ,"Drop values associated with certain variables."
  ,
  FMDeleteMultiple[f, {y}]
  ];

DefinePublicFunction[
FMDeleteMultiple[f_FiniteMapping, y_List] ,"", FMDomainDeleteCases[f, x_ /; MemberQ[y,x]]
  ];

DefinePublicFunction[
FMRuleCases[f_FiniteMapping, x_] ,"", FiniteMappingMakeFromRules@Cases[FMAsRules@f, x]
  ];

  DefinePublicFunction[
FMSelect[f_FiniteMapping, test_] ,"", FMCases[f, _?test]];

DefinePublicFunction[
FMCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] ,"",
     FiniteMappingMakeFromRules@Cases[FMAsRules@f, Rule[k_, pat] :> Rule[k, rep]]
  ];

DefinePublicFunction[
FMCases[f_FiniteMapping, pat_] ,"", FiniteMappingMakeFromRules@Cases[FMAsRules@f, _[k_, pat]]
  ];

DefinePublicFunction[
FMDeleteCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] ,"", 
    FiniteMappingMakeFromRules@DeleteCases[FMAsRules@f, Rule[k_, pat]]
  ];

DefinePublicFunction[
FMDomainSelect[f_FiniteMapping, test_] ,"", FMDomainCases[f, _?test]
];
DefinePublicFunction[
FMDomainCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] ,"", 
    FiniteMappingMakeFromRules@Cases[FMAsRules@f, Rule[pat, v_] :> Rule[rep, v]]
];
DefinePublicFunction[
FMDomainCases[f_FiniteMapping, pat_] ,"",  FiniteMappingMakeFromRules@Cases[FMAsRules@f, _[pat, _]]
  ];

DefinePublicFunction[
FMDomainDeleteCases[f_FiniteMapping, pat_] ,"",  FiniteMappingMakeFromRules@DeleteCases[FMAsRules@f, _[pat, _]]
  ];


(*
(* Joining matrices/lists efficiently *)

(* Joining a list of equally sized arrays to a matrix (vertically or horizontally) *)
Module[{isListOfSameLengthLists},
  isListOfSameLengthLists[f_FiniteMapping] := Module[{n = FMLength@f},
    AllTrue[FMEvaluateAll@f, Length@# == n&]
    ];

FMPairing[] :=

(* joining expressions/position indexed stuff efficiently: just place them in a nested list *)
*)

(* General case *)
(*/; VectorQ[FMEvaluateAll@f, _FiniteMapping]*) (* TODO add a way to claim that all of domain/range match a certain pattern (=== come from a computable set of expressions)*)

DefinePublicFunction[
FMPairing[f_FiniteMapping, pairing_ : List] /; AllTrue[FMEvaluateAll@f, FiniteMappingQ]
    ,"Given

f: K -> U_k (J_k -> L)

produce

g: K x U_k  J_k -> L

where \"x\" is defined by pairing"

    ,FMPairing[FMDomain@f, FMEvaluateAll@f, pairing]
];

DefinePublicFunction[
FMPairing[ks_List, fs : {___FiniteMapping}, pairing_ : List] /; Length@ks == Length@fs

  ,""
  ,    FMConcat@Table[pairing[ks[[i]],#]&~FMMapDomain~fs[[i]], {i,Length@ks}]
  ];
(*
    Array[

      Cases[
        FMAsRules@fs[[#]]
        , (j_ -> l_) :> (pairing[ks[[#]],j] -> l)
      ] &

      , Length@ks
    ] // Flatten // FiniteMappingMakeFromRules
    *)

DefinePublicFunction[
FMValuesMatrix[f_FiniteMapping]
  ,"Attempt to construct a matrix from this by:
- detecting first and second varname elements by using Cases with pairing
- DeleteDuplicates on these
- index into the result set using these -- or just assume the set is sorted as specified"

  ,f~FMValuesMatrix~Head@First@FMDomain@f
]; (* this will not be correct when Reverse was used !*)

DefinePublicFunction[
FMValuesMatrix [f_FiniteMapping, pairing_]
  ,"",

  Module[{
  varnames = DeleteDuplicates /@ Transpose@Cases[FMDomain@f, pairing[x_,y_] :> {x,y}],
  i, j, x, y
},
  {i,j} = varnames;
  Table[FMEvaluate[f,pairing[x,y]], {x,i},{y,j}]

(*ArrayReshape[RVVValues@v, Length /@ varnames]*) (* TODO this is not equivalent
  when v as just any ordering *) (* but it preserver sparsity..*)
]
  ];

DefinePublicFunction[
FMDomainMatrix [f_FiniteMapping] ,"", f~FMDomainMatrix~Head@First@FMDomain@f
  ];

DefinePublicFunction[
FMDomainMatrix [f_FiniteMapping, pairing_]
  ,"",
  Module[{
  varnames = DeleteDuplicates /@ Transpose@Cases[FMDomain@f, pairing[x_,y_] :> {x,y}],
  i, j, x, y
},
  {i,j} = varnames;
  Table[pairing[x,y], {x,i},{y,j}]

(*ArrayReshape[RVVValues@v, Length /@ varnames]*) (* TODO this is not equivalent
  when v as just any ordering *) (* but it preserver sparsity..*)
]
  ];

End[]
EndPackage[]
