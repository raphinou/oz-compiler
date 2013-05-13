% Named functor
local
   functor F
      export
         Echo
      import
         System(show:ImportedShow)
         DumpAST at '../lib/DumpAST.ozf'
      prepare
         Test=1000
      define
         {ImportedShow 'define::start'}
         proc {Echo S}
            {PrivateEcho S}
         end
         proc {PrivateEcho S}
            {ImportedShow Test}
            {DumpAST.dumpAST fRecord(fConst('hello' pos) [fColon(fConst(1 pos) fConst('one' pos)) fColon(fConst(2 pos) fConst('two' pos))]) _}
            {ImportedShow S}
         end
         {ImportedShow 'define::end'}
   end
   M
in
skip
   M={Module.apply [F]}.1
   {M.echo 'hello'}
end

