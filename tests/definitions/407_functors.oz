% Named functor with require
local
   functor F
      export
         Echo
      require
         % need path relative to this file
         DumpAST at '../../lib/DumpAST.ozf'
      import
         System(show:ImportedShow)
      prepare
         {Show 'will dump in prepare with required function'}
         {DumpAST.dumpAST fRecord(fConst('prepare' pos) [fColon(fConst(1 pos) fConst('one' pos)) fColon(fConst(2 pos) fConst('two' pos))]) _}
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

