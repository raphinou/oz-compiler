% Tuplespace and queues
local
   NewQueue
   Insert
   Delete
   DeleteNonBlock
   DeleteAll
   Size
   TupleSpace
   TS
in
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Declarative concurrent queue

   fun {NewQueue}
      X in
      q(0 X X)
   end

   fun {Insert q(N S E) X}
      E1 in
      E=X|E1 q(N+1 S E1)
   end

   fun {Delete q(N S E) X}
      S1 in
      S=X|S1 q(N-1 S1 E)
   end

   fun {DeleteNonBlock q(N S E) X}
      if N>0 then H S1 in
         X=[H] S=H|S1 q(N-1 S1 E)
      else
         X=nil q(N S E)
      end
   end

   fun {DeleteAll q(_ S E) L}
      X in
      L=S E=nil
        q(0 X X)
   end

   fun {Size q(N _ _)} N end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Tuple spaces

   class TupleSpace
      prop locking
      attr tupledict

      meth init tupledict:={NewDictionary} end

      meth EnsurePresent(L)
         if {Not {Dictionary.member @tupledict L}}
         then @tupledict.L:={NewQueue} end
      end

      meth Cleanup(Q L)
         @tupledict.L:=Q
         if {Size Q}==0
         then {Dictionary.remove @tupledict L} end
      end

      meth write(Tuple)
         lock L={Label Tuple} in
       {self EnsurePresent(L)}
       @tupledict.L:={Insert @tupledict.L Tuple}
         end
      end

      meth read(L Tuple) X in
         lock Q in
       {self EnsurePresent(L)}
       Q={Delete @tupledict.L X}
       {self Cleanup(Q L)}
         end
         {Wait X} X=Tuple
      end

      meth readnonblock(L Tuple ?B)
         lock U Q in
       {self EnsurePresent(L)}
       Q={DeleteNonBlock @tupledict.L U}
       case U of [X] then
          {self Cleanup(Q L)} B=true X=Tuple
       else B=false end
         end
      end
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Examples of tuple spaces

   {Show '------------------'}
   {Show '--  TUPLESPACE  --'}
   {Show '------------------'}


   TS={New TupleSpace init}

   {TS write(foo(1))}
   {TS write(foo(1 2))}
   {TS write(bar(2))}

   {Show {TS read(foo $)}}
   {Show {TS read(bar $)}}

   {TS write(foo(1 2 3))}
   {TS write(car(5))}
   {TS write(car(6))}

   {TS write(foo(a))}

   {Show {TS read(car $)}}
   {Show {TS read(foo $)}}
   {Show {TS read(foo $)}}

   % Implementing a queue using a tuple space

   {Show '-------------'}
   {Show '--  QUEUE  --'}
   {Show '-------------'}

   local
      NewQueue
   in
      proc {NewQueue Insert Delete}
         TS={New TupleSpace init}
         X
      in
	 {TS write(q(0 X X))}
	 proc {Insert X}
	    N S E in
	    {TS read(q q(N S X|E))}
	    {TS write(q(N+1 S E))}
	 end
	 proc {Delete X}
	    N S E in
	    {TS read(q q(N X|S E))}
	    {TS write(q(N-1 S E))}
	 end
      end

      local Ins Del in
	 {NewQueue Ins Del}
	 {Ins foo}
	 {Show {Del}}
	 {Ins bar}
	 {Show {Del}}
	 for I in 1..4 do
	 {Ins i(I)}
      end

      for I in 1..4 do
	 {Show {Del}}
      end
      end
   end
end
