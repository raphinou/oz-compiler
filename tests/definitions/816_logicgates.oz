% Logic gates simulations
local
   Inc
   WaitList
   And
   AndG
   GateMaker
   AndG
   OrG
   XorG
   FullAdder
   NbitAdder
   DelayG
   NotG
   Latch
   Clock
   Mystery


in
   fun lazy {Inc Xs}
      case Xs of X|Xr then
         (X+1)|{Inc Xr}
      end
   end
   fun {WaitList L}
      fun {WaitListInt L Lf}
         case Lf
         of X|Xs then
       {WaitListInt L Xs}
         [] nil then
       L
         end
      end
   in
      {WaitListInt L L}
   end


   % declare Xs Ys Zs in
   % thread Ys={Inc Xs} end
   % thread Zs={Inc Ys} end
   % thread Xs={Inc Zs} end
   % {Browse xs#Xs}

   local Xs Ys Zs in
   thread Ys={Inc Xs} end
   thread Zs={Inc 1|Ys} end
   thread Xs={Inc Zs} end
   {Show {List.take Xs 10}}
   end

   fun {And X Y} X*Y end

   % Lift the boolean function to streams

   local Xs Ys Zs in
      local
         fun {AndLoop Xs Ys}
            case Xs#Ys of (X|Xr)#(Y|Yr) then
          {And X Y}|{AndLoop Xr Yr}
            else
          nil
            end
         end
      in
         thread Zs={AndLoop Xs Ys} end
      end


   Xs=1|0|1|0|nil
   Ys=0|0|1|1|nil

   {Show xs#{WaitList Xs}}
   {Show ys#{WaitList Ys}}
   {Show zs#{WaitList Zs}}
   end


   fun {AndG Xs Ys}
      fun {AndLoop Xs Ys}
         case Xs#Ys of (X|Xr)#(Y|Yr) then
       {And X Y}|{AndLoop Xr Yr}
         else
       nil
         end
      end
   in
      thread {AndLoop Xs Ys} end
   end

   local A B C D E F in
   E={AndG C D}
   D={AndG A B}
   end

   % Or, Xor -> copy the AndG, replace And by other
   % functions
   % Another way: Make a generic function

   fun {GateMaker F}
      fun {GateG Xs Ys}
         fun {GateLoop Xs Ys}
       case Xs#Ys of (X|Xr)#(Y|Yr) then
          {F X Y}|{GateLoop Xr Yr}
       else
          nil
       end
         end
      in
         thread {GateLoop Xs Ys} end
      end
   in
      GateG
   end
   local AndG OrG XorG
   in
      AndG={GateMaker fun {$ X Y} X*Y end}
      OrG={GateMaker fun {$ X Y} X+Y-X*Y end}
      XorG={GateMaker fun {$ X Y} (X + Y) mod 2 end}


      {Show xor#{WaitList {XorG 0|1|0|1|nil 0|0|1|1|nil}}}

      % Full adder component
      proc {FullAdder X Y Ci S Co}
         A D E F G
      in
         A={XorG X Y}
         S={XorG A Ci}
         D={AndG X Y}
         E={AndG X Ci}
         F={AndG Y Ci}
         G={OrG D E}
         Co={OrG G F}
      end

      % N-bit adder using lists of streams
      proc {NbitAdder Xs Ys Zs Ci Co}
         case (Xs#Ys#Zs) of (X|Xr)#(Y|Yr)#(Z|Zr) then Cm in
            {FullAdder X Y Co Z Cm}
            {NbitAdder Xr Yr Zr Ci Cm}
         [] nil then Co=Ci
         end
      end

      fun {DelayG X}
         0|X
      end

      fun {NotG Xs}
         fun {NotLoop Xs}
            {Delay 100}
            case Xs of X|Xr then (1-X)|{NotLoop Xr} else nil end
         end
      in
         thread {NotLoop Xs} end
      end

      % Latch
      proc {Latch C Di Do}
         A B D E
      in
         B={AndG A Di}
         D={AndG E C}
         E={DelayG Do}
         Do={OrG D B}
         A={NotG C}
      end


      local C Di Do in
      {Latch C Di Do}

      C =0|0|0|0|1|1|1|0|0|0|0|1|1|0|nil
      Di=0|1|0|0|1|1|0|0|1|0|1|1|0|0|nil
      {Show c#{WaitList C}}
      {Show di#{WaitList Di}}
      {Show doo#{WaitList Do}}
      end

      % Clock circuit
      fun {Clock}
         1|{Clock}
      end

      %% Oscillator
      %declare A B in
      %B={DelayG A}
      %A={NotG B}
      %{Browse A}

      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      % Exam question Jan. 20, 2010
      % Mystery circuit is a rising edge-triggered toggle

      fun {Mystery Cin}
         Ci D1 D2 Cout
      in
         Ci={NotG Cin}
         {Latch Cin D1 D2}
         {Latch Ci D2 Cout}
         D1={NotG {DelayG Cout}}
         Cout
      end


      local Cin Cout in
      Cout={Mystery Cin}
      Cin=0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|nil
      {Show cin1#{WaitList Cin}}
      {Show cout1#{WaitList Cout}}
      end


      local Cin Cmid Cout Cout2 in


      Cmid={Mystery Cin}
      Cout={Mystery Cmid}
      Cout2={Mystery Cout}

      Cin=0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|1|0|nil

      {Show cin#{WaitList Cin}}
      {Show cmid#{WaitList Cmid}}
      {Show cout#{WaitList Cout}}
      {Show cout2#{WaitList Cout2}}
      end
   end
end

