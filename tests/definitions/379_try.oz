% try-finally without catch
try
   {Show hello}
finally
   {Show final}
end

try
   try
      raise error end
   finally
      {Show final}
   end
catch error then
   {Show 'caught error exception'}
[] E then
   raise E end
end

local
   V
in
   V=try
      5
   finally
      {Show final}
   end
   {Show V}
end


try
   local
      V
   in
      V=try raise error end finally {Show final} end
      {Show V}
   end
catch error then
   {Show 'error exception caught'}
[] E then
   raise E end
end
