local
   P1 A
in
   proc {P1 A11}
      P2 C
   in
      C=2
      proc {P2 A21}
         {Show A}
         {Show A}
         {Show C}
      end
      {P2 A11}
   end
   A=3
   {P1 A}
end
