% Pattern conjunctions open records and escaped variables in function arguments
local
   P C=c
in
   proc {P V=fRecord(lab [ f1(a B !C ...) f2 F3 ] M=more(m1 M2 ...) ... )}
      {Show V}
      {Show B}
      {Show F3}
      {Show M}
      {Show M2}
   end
   {P fRecord(lab [f1(a b c d e f) f2 f3] more(m1 m2 m3) rest)}
end
