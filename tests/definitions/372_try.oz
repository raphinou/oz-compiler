% Corner case illustrating the need of temp var when desugaring try expression
local
   proc {P X}
      X = 42
      raise exception end
   end

   Y =
   try {P}
   catch exception then
      error
   end
in
   {Show Y}
end

