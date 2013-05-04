% Lift simulation. Added determinism with controlled delays and lifts called
local
   NewPortObject2
   NewPortObject
   Timer
   Displayer
   Tid
   Dis
   Controller
   DelayTime=100
in
   fun {NewPortObject2 Proc}
      Sin in
      thread for Msg in Sin do {Proc Msg} end end
      {NewPort Sin}
   end

   % Longer version
   fun {NewPortObject Init Fun}
      proc {MsgLoop S1 State}
         case S1 of Msg|S2 then
       {MsgLoop S2 {Fun State Msg}}
         [] nil then skip end
      end
      Sin
   in
      thread {MsgLoop Sin Init} end
      {NewPort Sin}
   end


   /*
   % Shorter version
   % Sin = message stream
   % Out = output state
   fun {NewPortObject Init Fun}
      Sin Out in
      thread {FoldL Sin Fun Init Out} end
      {NewPort Sin}
   end
   */
   fun {Timer}
      {NewPortObject2
       proc {$ Msg}
          case Msg of starttimer(T Pid) then
        thread {Delay T} {Send Pid stoptimer} end
          end
       end}
   end

   fun {Displayer}
      {NewPortObject2
       proc {$ Msg}
          {Show Msg}
       end}
   end

   Tid={Timer}
   Dis={Displayer}

   {Send Dis start}
   {Send Tid starttimer(2*DelayTime Dis)}

   fun {Controller Init}
      Tid={Timer}
      Cid={NewPortObject Init
      fun {$ state(Motor F Lid) Msg}
         case Motor
         of running then
            case Msg
            of stoptimer then
                {Send Lid 'at'(F)}
                state(stopped F Lid)
            end
         [] stopped then
            case Msg
            of step(Dest) then
                if F==Dest then
                   state(stopped F Lid)
                elseif F<Dest then
                   {Send Tid starttimer(DelayTime Cid)}
                   state(running F+1 Lid)
                else % F>Dest
                   {Send Tid starttimer(DelayTime Cid)}
                   state(running F-1 Lid)
                end
            end
         end
      end}
   in Cid end

   local
      Dis={Displayer}
      Cid={Controller state(stopped 1 Dis)}
      Floor
      ScheduleLast
      Lift
      Building

   in
      {Send Dis startcontroller}
      {Send Cid step(5)}

      fun {Floor Num Init Lifts}
         Tid={Timer}
         Fid={NewPortObject Init
         fun {$ state(Called Calls) Msg}
            case Called
            of notcalled then Lran in
               case Msg
               of arrive(Ack) then
             {Show 'Lift at floor '#Num#': open doors'}
             {Send Tid starttimer(DelayTime*5 Fid)}
             state(doorsopen(Ack) Calls+1)
               [] call then
             {Show 'Floor '#Num#' calls lift '#(Calls+Num mod {Width Lifts})}
             Lran=Lifts.(Calls+Num mod {Width Lifts})
             {Send Lran call(Num)}
             state(called Calls+1)
               end
            [] called then
               case Msg
               of arrive(Ack) then
             {Show 'Lift at floor '#Num#': open doors'}
             {Send Tid starttimer(5*DelayTime Fid)}
             state(doorsopen(Ack) Calls+1)
               [] call then
             state(called Calls+1)
               end
            [] doorsopen(Ack) then
               case Msg
               of stoptimer then
             {Show 'Lift at floor '#Num#': close doors'}
             Ack=unit
             state(notcalled Calls+1)
               [] arrive(A) then
             A=Ack
             state(doorsopen(Ack) Calls+1)
               [] call then
             state(doorsopen(Ack) Calls+1)
               end
            end
         end}
      in Fid end

      fun {ScheduleLast L N}
         if L\=nil andthen {List.last L}==N then L
         else {Append L [N]} end
      end

      fun {Lift Num Init Cid Floors}
         {NewPortObject Init
          fun {$ state(Pos Sched Moving) Msg}
             case Msg
             of call(N) then
                {Show 'Lift '#Num#' needed at floor '#N}
                if N==Pos andthen {Not Moving} then
                   {Wait {Send Floors.Pos arrive($)}}
                   state(Pos Sched false)
                else Sched2 in
                   Sched2={ScheduleLast Sched N}
                   if {Not Moving} then
                 {Send Cid step(N)} end
                   state(Pos Sched2 true)
                end
             [] 'at'(NewPos) then
                {Show 'Lift '#Num#' at floor '#NewPos}
                case Sched
                of S|Sched2 then
                   if NewPos==S then
                      {Wait {Send Floors.S arrive($)}}
                      if Sched2==nil then
                         state(NewPos nil false)
                      else
                         {Send Cid step(Sched2.1)}
                         state(NewPos Sched2 true)
                      end
                   else
                      {Send Cid step(S)}
                      state(NewPos Sched Moving)
                   end
                end
             end
          end}
      end


      proc {Building FN LN ?Floors ?Lifts}
         Lifts={MakeTuple lifts LN}
         for I in 1..LN do Cid in
            Cid={Controller state(stopped 1 Lifts.I)}
            Lifts.I={Lift I state(1 nil false) Cid Floors}
         end
         Floors={MakeTuple floors FN}
         for I in 1..FN do
            Floors.I={Floor I state(notcalled 0) Lifts}
         end
      end

      local F L in
         {Building 30 6 F L}

         {Delay 1000}
         {Send L.1 call(5)}
         {Delay 1000}

         {Send L.1 call(1)}
         {Delay 1000}

         {Send F.9 call}
         {Delay 1000}
         {Send L.2 call(6)}
         {Delay 1000}

         {Send F.10 call}
         {Delay 1000}
         {Send L.1 call(4)}
         {Delay 1000}
         {Send L.2 call(5)}
         {Delay 1000}
         {Send F.1 call}
         {Delay 1000}
         {Send F.2 call}
         {Delay 1000}

         {Send L.1 call(30)}
         {Delay 1000}
         {Send L.1 call(1)}
         {Delay 1000}
         {Send L.1 call(29)}
         {Delay 1000}
         {Send L.1 call(2)}
         {Delay 1000}
      end
   end
end


