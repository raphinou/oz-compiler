% Check prepare is executed when functor is applied
local
   F=functor
      export
         echo:Echo
      import
         System(show:ImportedShow)
      prepare
         Test=1000
      define
         {ImportedShow 'define::start'}
         proc {Echo S}
            {PrivateEcho S}
         end
         proc {PrivateEcho S}
            {ImportedShow Test}
            {ImportedShow S}
         end
         {ImportedShow 'define::end'}
   end
   M
in
skip
   M={Module.apply [F]}.1
end

