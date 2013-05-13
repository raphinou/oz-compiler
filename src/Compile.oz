functor

import
   Narrator('class')
   ErrorListener('class')
   System(showInfo show:Show)
   DumpAST at '../lib/DumpAST.ozf'
   Module
   Helpers( symbol:Symbol
            syntheticSymbol:SyntheticSymbol
            wrapInFAnd:WrapInFAnd
            wrapIn:WrapIn
            unWrapFAnd:UnWrapFAnd
            listToAST:ListToAST
            indexOf:IndexOf
            firstOfP:FirstOfP
            genLabel:GenLabel
            getPos:GetPos
            defaultPass:DefaultPass
            defaultPassNoParams:DefaultPassNoParams
            storeInSafe:StoreInSafe
            accessSafe:AccessSafe
            isSafe:IsSafe
            ) at '../lib/Helpers.ozf'
   NamerFunction(namer:Namer) at '../lib/Namer.ozf'
   DeclsFlattenerFunction(pVS:PVS pVE:PVE declsFlattener:DeclsFlattener) at '../lib/DeclsFlattener.ozf'
   DesugarFunction(desugar:Desugar) at '../lib/Desugar.ozf'
   UnnesterFunction(unnester:Unnester) at '../lib/Unnester.ozf'
   GlobaliserFunction(globaliser:Globaliser) at '../lib/Globaliser.ozf'
   CodeGenFunction(codeGen:CodeGen) at '../lib/CodeGen.ozf'
   % for nested environments debugging
   OS

export
   namer: Namer
   genCode: CodeGen
   declsFlattener: DeclsFlattener
   desugar: Desugar
   unnester: Unnester
   globaliser: Globaliser
   pve: PVE
   pvs: PVS
   unWrapFAnd: UnWrapFAnd
define



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Actual work happening
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   skip


end
