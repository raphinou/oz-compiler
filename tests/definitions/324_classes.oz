% Dynamic attribute access
local
   class Accessors
      meth set(Attr Value)
         Attr:=Value
      end
      meth get(Attr ?R)
         R=@Attr
      end
   end
   class Symbol from Accessors
      attr
        id
         name
         pos
      meth init(Id Name Pos)
         id:=Id
         name:=Name
         pos:=Pos
      end
   end
   I={New Symbol init(1 'S1' pos(1 2))}
in
   {Show {I get(pos $)}}
   {Show {I get(name $)}}
   {I set(name 'S2')}
   {Show {I get(name $)}}
end

