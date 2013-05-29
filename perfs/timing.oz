functor
   import
      System
      Boot_Time at 'x-oz://boot/Time'
define
   T0 T1
    fun {Fact N}
      if N>1 then
          N*{Fact N-1}
      else
          1
      end
   end

   fun{Fib N}
      fun{Loop N A B}
         if N == 0 then
            B
         else
            {Loop N-1 A+B A}
         end
      end
   in
      {Loop N 1 0}
   end

   fun{SlowFib N}
      if N==0 then 0
      elseif N==1 then 1
      else
         {SlowFib N-2}+{SlowFib N-1}
      end
   end

%proc {CellAccess N}
%   C={NewCell 0}
%   in
%      for I in 1..N do
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%         C:=@C+I
%      end
%   end
%proc {CellRead N}
%   C={NewCell 0}
%   in
%      for I in 1..N do
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%         _=@C
%      end
%   end
%
%proc {CellWrite N}
%   C={NewCell 0}
%   in
%      for I in 1..N do
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%         C:=1
%      end
%   end

%   proc {Addition N}
%      for I in 1..N do
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%         _=1+1
%      end
%   end
%
%   proc {Rec N}
%      for I in 1..N do
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%         _ = rec(a:1 b:2)
%      end
%   end
%
%   proc {PatMatch A N}
%      for I in 1..N do
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] 2 then
%            skip
%         end
%      end
%   end
%   proc {OpenMatch A N}
%      for I in 1..N do
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%         case A
%         of 1 then
%            skip
%         [] rec(a:_ ...) then
%            skip
%         [] rec(b:_ ...) then
%            skip
%         [] rec(c:_ ...) then
%            skip
%         [] rec(d:_ ...) then
%            skip
%         [] rec(e:_ ...) then
%            skip
%         [] rec(f:_ ...) then
%            skip
%         [] 2 then
%            skip
%         end
%      end
%   end
in
   {System.gcDo}
   T0={Boot_Time.getReferenceTime}


   {SlowFib 30 _}
   %{CellAccess 100000}
   %{CellRead 100000}
   %{CellWrite 100000}
   %{Addition 100000}
   %{System.show {Addition2 100000} }
   %{Rec 1000000}
   %{PatMatch 2 1000000}
   %{OpenMatch 2 1000000}
   %{OpenMatch rec(f:1 z:3 y:4 x:6) 100000}


   T1={Boot_Time.getReferenceTime}
   {System.show T1-T0}
end

