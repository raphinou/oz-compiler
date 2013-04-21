% Exchange operation on attribute, and assigning to an attribute with no default value.
local
   S I
in
   class S
      attr
         status
      meth init()
         status:=1
      end
      meth transition()
         Tmp
      in
         Tmp=(status:=@status+1)
         {Show 'Transisitioned from '#Tmp#' to '#@status}
      end
   end
   I = {New S init()}
   {I transition()}
   {I transition()}
end


