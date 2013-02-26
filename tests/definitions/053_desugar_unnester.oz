% Cell creation, assignment, access, exchange, cell in cell
local
   A={NewCell 0}
   B={NewCell 0}
   C={NewCell 2}
   D={NewCell {NewCell 0}}
   I
   OldB
in
   B:=1
   @D:=3
   {Show @A}
   {Show @B}
   {Show @C}
   {Show @@D}
   OldB=(B:=4)
   {Show @A}
   {Show OldB}
   {Show @C}
   {Show @@D}
   {Show @B}
end
