% Computes the K minimal elements of a list.  This program compares
% eager algorithms with a simple lazy algorithm.
local
   Measure
   Shuffle
   ExtractMin
   SelectMinimal
   NewHeap
   HeapMinimal
   Append
   QuickSort
   QuickMinimal
   QuickSortN
   ExplicitQuickMinimal
   Pow
in


%% returns the execution time of {P}
fun {Measure P}
   T0
   T1
in
   %T0={Property.get time}
   T0={Time.time}
   {P}
   %T1={Property.get time}
   T1={Time.time}
   %(T1.user+T1.system)-(T0.user+T0.system)
   T1-T0
end

local
   %% represents the list Xs as a balanced tree
   fun {TreeList Xs} {MakeTreeList Xs {Length Xs} nil} end
   fun {MakeTreeList Xs N ?Xr} % Xr={Drop Xs N}, N>0
      if N<2 then X in Xs=X|Xr
    single(X size:{NewCell 1})
      else N1=(N div 2) R in
    list(size:{NewCell N} {MakeTreeList Xs N1 R} {MakeTreeList R N-N1 Xr})
      end
   end
   %% removes and returns the Nth element of the tree list TL
   fun {RemoveNth TL N}
      case TL
      of single(X size:S) andthen N==1 then S:=0 X
      [] list(L1 L2 size:S) then N1=@(L1.size) in S:=@S-1
    if N=<N1 then {RemoveNth L1 N} else {RemoveNth L2 N-N1} end
      end
   end
   %% randomly picks the elements of TL
   fun {ShuffleTreeList TL}
      case @(TL.size)
      of 0 then nil
      [] N then {RemoveNth TL ({OS.rand} mod N + 1)}|{ShuffleTreeList TL}
      end
   end
in
   %% shuffles the list Xs in O(N log N), where N={Length Xs}
   fun {Shuffle Xs}
      if Xs==nil then nil else {ShuffleTreeList {TreeList Xs}} end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local
   %% This function optimizes the removal of the minimum while looking
   %% for it (CurXs#CurXt is a difference list).
   proc {Extract Xs M Xr CurM CurXr CurXt}
      case Xs
      of X|T andthen X<CurM then Xr=CurM|CurXr {ExtractMin Xs M CurXt}
      [] X|T then CurXt=X|{Extract T M Xr CurM CurXr $}
      [] nil then M=CurM Xr=CurXr CurXt=nil
      end
   end
in
   %% returns the minimum M of Xs, and Xr, which is Xs with M removed
   proc {ExtractMin Xs ?M ?Xr}
      case Xs of X|T then L in {Extract T M Xr X L L} else skip end
   end
end

%% returns the K minimal elements of Xs (K selections of min)
%% complexity: O(KN), where N={Length Xs}
fun {SelectMinimal Xs K}
   if K>0 then M Xr in
      {ExtractMin Xs M Xr} M|{SelectMinimal Xr K-1}
   else nil end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% creates a heap with maximum N elements (top is maximum)
fun {NewHeap N}
   Size={NewCell 0}
   Heap={NewArray 1 N unit}
   proc {Exchange N M} X in X=Heap.N:=(Heap.M:=X) end
   proc {CheckUp N}
      if N>1 then Parent=N div 2 in
    if Heap.N>Heap.Parent then {Exchange N Parent} {CheckUp Parent} end
      end
   end
   proc {CheckDown N}
      C1#C2=(2*N)#(2*N+1) % left and right children
   in
      if C1>@Size then % no children
    skip
      elseif C2>@Size orelse Heap.C1>Heap.C2 then % check left child
    {CheckDownTwo N C1}
      else % check right child
    {CheckDownTwo N C2}
      end
   end
   proc {CheckDownTwo N C} % check a N with its child C
      if Heap.C>Heap.N then {Exchange C N} {CheckDown C} end
   end
in
   heap(isEmpty: fun {$} @Size==0 end
   insert: proc {$ X}
         N N1 in N=Size:=N1 N1=N+1 Heap.N1:=X {CheckUp N1}
      end
   remove: proc {$ X}
         N N1 in N=Size:=N1 N1=N-1 X=Heap.1:=Heap.N {CheckDown 1}
      end
       )
end

%% returns the K minimal elements of Xs (using a heap of size K+1)
%% complexity: O((N+K) log K), where N={Length Xs}
fun {HeapMinimal Xs K}
   Heap={NewHeap K+1}
   fun {Elements Stack}
      if {Heap.isEmpty} then Stack else {Elements {Heap.remove}|Stack} end
   end
   Xs1 Xs2={List.takeDrop Xs K Xs1}
in
   for X in Xs1 do {Heap.insert X} end
   for X in Xs2 do {Heap.insert X} {Heap.remove _} end
   {Elements nil}
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun lazy {Append Xs Ys}
   case Xs of X|Xr then X|{Append Xr Ys} else Ys end
end
fun lazy {QuickSort Xs}
   case Xs of X|Xr then L1 L2 in
      {List.partition Xr fun {$ Y} Y<X end L1 L2}
      {Append {QuickSort L1} X|{QuickSort L2}}
   else nil end
end

%% returns the K minimal elements of Xs (with lazy quicksort)
%% complexity: O(N + K log K) on average, where N={Length Xs}
fun {QuickMinimal Xs K}
   {List.take {QuickSort Xs} K}
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local
   Append=List.append
   fun {QuickSort Xs}
      case Xs of X|Xr then L1 L2 in
    {List.partition Xr fun {$ Y} Y<X end L1 L2}
    {Append {QuickSort L1} X|{QuickSort L2}}
      else nil end
   end
in
   %% returns the N first elements of Xs sorted
   fun {QuickSortN Xs N}
      case Xs of X|Xr then L1 L2 in
    {List.partition Xr fun {$ Y} Y<X end L1 L2}
    if {Length L1}>=N then % enough elements in L1, don't sort L2
       {QuickSortN L1 N}
    else
       {List.take {Append {QuickSort L1} X|{QuickSort L2}} N}
    end
      else nil end
   end
end

%% returns the K minimal elements of Xs (with "limited" quicksort)
%% complexity: O(N + K log K) on average, where N={Length Xs}
fun {ExplicitQuickMinimal Xs K}
   {QuickSortN Xs K}
end
fun {Pow N Exp}
   if Exp>0 then
      N*{Pow N Exp-1}
   else
      1
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
local
   Ls={Map {List.number 2 3 1}
       fun {$ E} {Shuffle {List.number 1 {Pow 10 E} 1}} end}
   proc {Test Label Minimal}
      for L in Ls do
	 {Show Label({Measure proc {$} {Minimal L 100 _}  end})}
      end
   end
in
   {Test select SelectMinimal}
   {Test heap HeapMinimal}
   {Test quick QuickMinimal}
   {Test explicitQuick ExplicitQuickMinimal}
end
end

