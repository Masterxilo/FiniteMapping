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

BeginPackage["FiniteMapping`", {"paul`"}]

FiniteMappingQ

FiniteMappingMakeFromRules::usage =
    "FiniteMappingFromRules[_[_[_,_]...]]
List of rules can be provided any 'list' of pairs (a,b).
Patterns in left-hand sides are treated as Verbatim";

FiniteMappingMakeConstant
FMMakeListDomainNames
FiniteMappingMakeFromList
FMForgetDomain
FiniteMappingMakeFromLists
FiniteMappingMakeFromArray
FiniteMappingMakeFromExpression
FMDomain
FMAsRules
FMAsAssociation
FMEvaluate
FMEvaluateIndexed
FMEvaluateMultiple
FMEvaluateIndexedMultiple
FMEvaluateAll
FMInjectiveQ
FMLength
FMInverse
FMGeneralizedInverse
FMCompose
FMMap
FMMapKeyed
FMMapValues
FMMapDomain
FMConcat
FMUpdate
FMDomainSubset
FMDeleteOne
FMDeleteMultiple
FMRuleCases
FMSelect
FMCases
FMDeleteCases
FMDomainSelect
FMDomainCases
FMDomainDeleteCases
FMPairing
FMValuesMatrix
FMDomainMatrix
FMUniverse
FMRange
FMUniverseRange

FiniteMapping

Begin["`Private`"]

FMUniverseRange[f_FiniteMapping] := Through[{FMUniverse, FMRange}@f];

FiniteMappingQ[_FiniteMapping] := True;
FiniteMappingQ[_] := False;
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


(* -- Purpose -- *)
(*
Creates a new finite mapping from a list of "rules"
*)


FiniteMappingMakeFromRules[ruleset : _[_[_,_]...]] :=
With[{a = Association@Evaluate[List@@Rule@@@ruleset]},
    FiniteMapping[Rules, a]
]

(* ^^ End ^^ *)


(* -- Purpose -- *)
(*
Creates a new finite mapping from parts and position specifications of an expression.
*)

(* TODO handle non-numeric constants. Can improve memory efficiency by special-casing this. *)
FiniteMappingMakeConstant[vars_List, val_] := FiniteMappingMakeFromLists[vars, SparseArray[{}, Length@vars, val]]


FMMakeListDomainNames[length_Integer] := Array[List, length]; (* this will be the FMDomain of a list of length
This can be considered a default naming scheme, used in case the user does not care.*)

FiniteMappingMakeFromList[l_List] := FiniteMappingMakeFromArray[l, 1];

FMForgetDomain[f_FiniteMapping] := FiniteMappingMakeFromList@FMEvaluateAll@f;
FMForgetDomain[f : FiniteMapping[Array | Expression, ___]] := f; (* TODO this is a noop anyhow - bug if called? On the other hand, this is not the right implementation: the domain will still be multi-dimensional after this *)

FiniteMappingMakeFromLists[a_List, fa : _List | _SparseArray?VectorQ] /; Length@a == Length@fa && DuplicateFreeQ@a := With[{vpf = PositionFunction@a},
  FiniteMapping[Lists, a, fa, vpf]
  ]

FiniteMapping::error = "``"
FiniteMappingMakeFromLists[x___] := (Message[FiniteMapping::error, {"FiniteMappingMakeFromLists called with wrong arguments",x}];$Failed)

FiniteMappingMakeFromArray[array_?ArrayQ] := FiniteMappingMakeFromArray[array, ArrayDepth@array];
FiniteMappingMakeFromArray[array_, level_Integer /; level > 0] /; IsArrayAtLevel[array, level]:=
        FiniteMapping[Array, level, array]

(* TODO support Heads->True *)
FiniteMappingMakeFromExpression[e_, levelspec_] := FiniteMapping[Expression, levelspec, e]

(* ^^ End ^^ *)

FMUniverseRange[f_FiniteMapping] := {_,_}

FMUtilGuessPattern[elements_List]

(* whether everything that matches p1 also matches p2 *)
(* heuristic only *)
FMUtilPatternSubset[p1_, p2_]

(* -- Purpose -- *)
(*
A

Similar to Keys@Association
*)

FMDomain[f : FiniteMapping[Rules, assoc_Association]] := Keys@assoc;
FMDomain[f : FiniteMapping[Lists, a_, fa_, vpf_]] := a;
FMDomain[f : FiniteMapping[Expression, levelspec_, e_]] := PositionsOnLevel[e, levelspec];
FMDomain[f : FiniteMapping[Array, level_, array_]] := Array[List@##&,Dimensions[array][[1;;level]]]~Flatten~(level-1);

(* -- Purpose -- *)
(*
As a list of rules
*)

FMAsRules[f : FiniteMapping[Rules, assoc_Association]] := Normal@assoc;
(*FMAsRules[f : FiniteMapping[Array, level_, array_]] /; TensorRank@array == level := ArrayRules@array (* TODO only works for level === TensorRank , adds a default rule, does not include 0*)*)
FMAsRules[f_FiniteMapping] := Thread[Rule[FMDomain@f, FMEvaluateAll[f]]];

FMAsRules[x___] := (Message[FiniteMapping::error, {"FMAsRules called with wrong arguments",x}];$Failed)

FMAsAssociation[f : FiniteMapping[Rules, assoc_Association]] := assoc;
FMAsAssociation[f_FiniteMapping] := Association@FMAsRules@f;

(* -- Purpose -- *)
(*
As an expression when A is a (sparse) list of position indices
Fill missing values with whatever is provided, use List or user specified head where unavailable.

TODO
*)


(* -- Purpose -- *)
(*
f(x)
*)

FMEvaluate[f : FiniteMapping[Rules, assoc_Association], x_] := assoc~Lookup~Key@x;
FMEvaluate[f : FiniteMapping[Lists, a_, fa_, vpf_], x_] := fa~Extract~vpf@x;
FMEvaluate[f : FiniteMapping[Expression, levelspec_, e_], x_] := e~Extract~x;
FMEvaluate[f : FiniteMapping[Array, level_, array_], x_] := array~Extract~x;

(* -- Purpose -- *)
(*
f(x) where x is specified via an index into A
*)
FMEvaluateIndexed[f : FiniteMapping[Rules, assoc_Association], x_Integer] := assoc[[x]];

FMEvaluateIndexed[f : FiniteMapping[Array, level_, array_], x_Integer] := array~Extract~FlatIndexToPosition[x,Dimensions[array][[1;;level]]];

FMEvaluateIndexed[f_FiniteMapping, x_Integer] := FMEvaluateAll[f][[x]]; (* TODO can we do better for Expression? At least keep stuff held... *)

(* -- Purpose -- *)
(*
f(A')

in a list of the same order
*)
FMEvaluateMultiple[f : FiniteMapping[Rules, assoc_Association], xs_List] :=  assoc~Lookup~xs;
FMEvaluateMultiple[f : FiniteMapping[Lists, a_, fa_, vpf_], xs_List] := fa~Extract~(List@*vpf/@xs); (* Note: must wrap a list around single integer position specification *)
FMEvaluateMultiple[f : FiniteMapping[Expression, levelspec_, e_], xs_List] := e~Extract~xs;
FMEvaluateMultiple[f : FiniteMapping[Array, level_, array_], xs_List] := array~Extract~xs;

(* -- Purpose -- *)
(*
f(A') where A' is specified as an index subset
*)
FMEvaluateIndexedMultiple[f : FiniteMapping[Rules, assoc_Association], xs : {___Integer}] := Values@assoc[[xs]];
FMEvaluateIndexedMultiple[f : FiniteMapping[Lists, a_, fa_, vpf_], xs : {___Integer}] := fa[[xs]];

FMEvaluateIndexedMultiple[f_FiniteMapping, xs_List] :=FMEvaluateIndexed[f, #] & /@ xs;


(* -- Purpose -- *)
(*
f(A)

in a list of the same order as FMDomain@f

This is similar to Values@Association
*)
FMEvaluateAll[f : FiniteMapping[Rules, assoc_Association]] := Values@assoc;
FMEvaluateAll[f : FiniteMapping[Lists, a_, fa_, vpf_]] := fa;
FMEvaluateAll[f : FiniteMapping[Array, level_, array_]] := Flatten[array, level-1];
FMEvaluateAll[f_FiniteMapping] := FMEvaluateMultiple[f, FMDomain@f];

(* -- Purpose -- *)
(*
Whether f is injective (bijective)
*)

FMInjectiveQ[f_FiniteMapping] := 
    Length@FMDomain@f === Length@DeleteDuplicates@FMEvaluateAll[f]

(* -- Purpose -- *)
(*
|A|
*)

FMLength[f : FiniteMapping[Array, level_, array_]] := Times@@Dimensions[array][[1;;level]];
FMLength[f_FiniteMapping] := Length@FMDomain@f

(* -- Purpose -- *)
(*
f^-1 for injective f 
*)

FMInverse[f_FiniteMapping?FMInjectiveQ] := FiniteMappingMakeFromRules@Thread@Rule[
FMEvaluateAll@f, FMDomain@f
];

(* -- Purpose -- *)
(*
Returns g such that g(y) is a list with
x in g(y) iff f(x) = y

This can be computed for any f.
*)
FMGeneralizedInverse[f : FiniteMapping[Rules, assoc_Association]] := With[{a=
    Association@Evaluate@PositionIndex@assoc},
 FiniteMapping[Rules, a]
]

FMGeneralizedInverse[f_FiniteMapping] :=
FMGeneralizedInverse@FiniteMappingMakeFromRules@FMAsRules@f

(* -- Purpose -- *)
(*
f°g
*)
FMCompose[f_FiniteMapping, g_FiniteMapping] := With[{a = FMDomain@g
  , fga = f~FMEvaluate~(g~FMEvaluate~#) & /@ FMDomain@g
  },
  FiniteMappingMakeFromLists[a,fga]
]

(* -- Purpose -- *)
(*
Like Map, but the result is a FiniteMapping instead of a List, indexed by list indices.
*)
FMMap[f_, e_List] := FiniteMappingMakeFromList@Map[f, e];

(* -- Purpose -- *)
(*
Like AssociationMap, but the result is a FiniteMapping
*)
FMMapKeyed[f_, e_] := With[{a = AssociationMap[f, e]},
  FiniteMapping[Rules, a]
]

(* -- Purpose -- *)
(*
Apply h to each value
*)
(* TODO can do much better in most cases *)
FMMapValues[h_, f: FiniteMapping[Rules, assoc_Association]] := {hassoc = h /@ assoc}~With~FiniteMapping[Rules, hassoc]
FMMapValues[h_, f: FiniteMapping[Lists, a_, fa_, vpf_]] := {hfa = h /@ fa}~With~FiniteMapping[Lists, a, hfa, vpf]
FMMapValues[h_, f: FiniteMapping[Array, level_, array_]] := {harray = Map[h, array, {level}]}~With~FiniteMapping[Array, level, harray]

FMMapValues[h_, f_FiniteMapping] := FiniteMappingMakeFromLists[FMDomain@f, h /@ FMEvaluateAll@f];

(* -- Purpose -- *)
(*
Apply f to each A to produce A'
*)
FMMapDomain[f_, g_FiniteMapping] := FiniteMappingMakeFromLists[f /@ FMDomain@g, FMEvaluateAll@g]; (* TODO can do better when keys are explicitly stored (Rules, Lists)*)

(* -- Purpose -- *)
(*
Produce a finiteMapping with the values of f followed by those of g.
Assumes f and g have disjoint variable names.
Use e.g. FMMapDomain to ensure this

FMConcat[f_FiniteMapping, g_FiniteMapping] :=
*)
FMConcat[l : {__FiniteMapping}] :=
    FiniteMappingMakeFromLists[
      Join@@(FMDomain /@ l),
      Join@@(FMEvaluateAll /@ l)
    ];

(* TODO can do better when keys are explicitly stored (Rules, Lists)*)



(* -- Purpose -- *)
(*
Update or add certain values.

Prefers values in g
*)

FMUpdate[f_FiniteMapping, g_FiniteMapping] := FiniteMappingMakeFromRules[FMAsRules@f~UpdateRuleList~FMAsRules@g]

(* -- Purpose -- *)
(*
Keep only values associated with certain a in A.
*)
FMDomainSubset[f_FiniteMapping, y_List] := FMDomainSelect[f, MemberQ[y,#]&]


(* -- Purpose -- *)
(*
Drop values associated with certain variables.
*)
FMDeleteOne[f_FiniteMapping, y_] := FMDeleteMultiple[f, {y}]

FMDeleteMultiple[f_FiniteMapping, y_List] := FMDomainDeleteCases[f, x_ /; MemberQ[y,x]]


FMRuleCases[f_FiniteMapping, x_] := FiniteMappingMakeFromRules@Cases[FMAsRules@f, x]


FMSelect[f_FiniteMapping, test_] := FMCases[f, _?test];
FMCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] := FiniteMappingMakeFromRules@Cases[FMAsRules@f, Rule[k_, pat] :> Rule[k, rep]]
FMCases[f_FiniteMapping, pat_] := FiniteMappingMakeFromRules@Cases[FMAsRules@f, _[k_, pat]]

FMDeleteCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] := FiniteMappingMakeFromRules@DeleteCases[FMAsRules@f, Rule[k_, pat]]


FMDomainSelect[f_FiniteMapping, test_] := FMDomainCases[f, _?test];
FMDomainCases[f_FiniteMapping, RuleDelayed[pat_, rep_]] := FiniteMappingMakeFromRules@Cases[FMAsRules@f, Rule[pat, v_] :> Rule[rep, v]]
FMDomainCases[f_FiniteMapping, pat_] :=  FiniteMappingMakeFromRules@Cases[FMAsRules@f, _[pat, _]]

FMDomainDeleteCases[f_FiniteMapping, pat_] :=  FiniteMappingMakeFromRules@DeleteCases[FMAsRules@f, _[pat, _]]

(* -- Purpose -- *)
(*
Given

f: K -> U_k (J_k -> L)

produce

g: K x U_k  J_k -> L

where "x" is defined by pairing
*)

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
FMPairing[f_FiniteMapping, pairing_ : List] /; AllTrue[FMEvaluateAll@f, FiniteMappingQ] := FMPairing[FMDomain@f, FMEvaluateAll@f, pairing];


FMPairing[ks_List, fs : {___FiniteMapping}, pairing_ : List] /; Length@ks == Length@fs :=
    FMConcat@Table[pairing[ks[[i]],#]&~FMMapDomain~fs[[i]], {i,Length@ks}]
(*
    Array[

      Cases[
        FMAsRules@fs[[#]]
        , (j_ -> l_) :> (pairing[ks[[#]],j] -> l)
      ] &

      , Length@ks
    ] // Flatten // FiniteMappingMakeFromRules
    *)



(* -- Purpose -- *)
(*
Attempt to construct a matrix from this by:
- detecting first and second varname elements by using Cases with pairing
- DeleteDuplicates on these
- index into the result set using these -- or just assume the set is sorted as specified
*)

FMValuesMatrix[f_FiniteMapping] := f~FMValuesMatrix~Head@First@FMDomain@f; (* this will not be correct when Reverse was used !*)

FMValuesMatrix [f_FiniteMapping, pairing_]  := Module[{
  varnames = DeleteDuplicates /@ Transpose@Cases[FMDomain@f, pairing[x_,y_] :> {x,y}],
  i, j, x, y
},
  {i,j} = varnames;
  Table[FMEvaluate[f,pairing[x,y]], {x,i},{y,j}]

(*ArrayReshape[RVVValues@v, Length /@ varnames]*) (* TODO this is not equivalent
  when v as just any ordering *) (* but it preserver sparsity..*)
];

FMDomainMatrix [f_FiniteMapping] := f~FMDomainMatrix~Head@First@FMDomain@f;

FMDomainMatrix [f_FiniteMapping, pairing_]  := Module[{
  varnames = DeleteDuplicates /@ Transpose@Cases[FMDomain@f, pairing[x_,y_] :> {x,y}],
  i, j, x, y
},
  {i,j} = varnames;
  Table[pairing[x,y], {x,i},{y,j}]

(*ArrayReshape[RVVValues@v, Length /@ varnames]*) (* TODO this is not equivalent
  when v as just any ordering *) (* but it preserver sparsity..*)
];

End[]
EndPackage[]