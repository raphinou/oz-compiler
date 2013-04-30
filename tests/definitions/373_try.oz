% try..finally statement
try {Show a} raise stopHere end {Show b}
catch
   stopHere then
   {Show 'stopped'}
finally
   {Show 'and finished'}
end
