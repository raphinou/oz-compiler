% Functor without import
local
   F=functor
      export
         echo:Echo
      define
         {Show 'define::start'}
         proc {Echo S}
            {PrivateEcho S}
         end
         proc {PrivateEcho S}
            {Show S}
         end
         {Show 'define::end'}
   end
   M
in
skip
   M={Module.apply [F]}.1
   {M.echo 'hello'}
end

