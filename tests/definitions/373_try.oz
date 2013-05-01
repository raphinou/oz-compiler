% try..finally statement

% Exception raised
try {Show a} raise stopHere end {Show b}
catch
   stopHere then
   {Show 'stopped'}
finally
   {Show 'and finished'}
end

% Success
try {Show a} {Show b}
catch
   stopHere then
   {Show 'stopped'}
finally
   {Show 'and finished'}
end
