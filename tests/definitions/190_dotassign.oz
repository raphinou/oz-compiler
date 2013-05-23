% Test dotAssign expressions
local
   proc {Exchange Heap N M}
      X
   in
      X=Heap.N:=(Heap.M:=X)
   end
   A={NewArray 1 2 0}
in
   A.1:=1
   A.2:=2
   {Exchange A 1 2}
   {Show A.1}
   {Show A.2}
end
