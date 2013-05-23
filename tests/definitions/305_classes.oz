% Class feature access
local
   C I
in
   class C
      feat
         type:circle
         volume:false
      attr
         status:0
         surface:0
      meth init()
         skip
      end
   end
   I = {New C init()}
   {Show I.type}
end


