% functor without prepare
local
   F=functor
      export
         echo:Echo
      import
         System(show:ImportedShow)
      define
         {ImportedShow 'define::start'}
         proc {Echo S}
            {PrivateEcho S}
         end
         proc {PrivateEcho S}
            {ImportedShow S}
         end
         {ImportedShow 'define::end'}
   end
   M
in
   M={Module.apply [F]}.1
   {M.echo 'hello'}
end

