% try-finally expressions
local
   V
in

   V=try raise stopHere end b
   catch
      stopHere then
      'stopped'
   finally
      {Show 'and finished'}
   end
   {Show '--'}
   {Show V}
   {Show '--'}

end

