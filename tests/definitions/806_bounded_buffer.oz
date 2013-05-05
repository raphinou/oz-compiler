% Bounded buffer, also using monitor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local
   NewQueue Insert Delete DeleteNonBlock DeleteAll Size
   NewMonitor
   Buffer
   B


in

   fun {NewQueue}
      X in
      q(0 X X)
   end

   fun {Insert q(N S E) X}
      E1 in
      E=X|E1 q(N+1 S E1)
   end

   fun {Delete q(N S E) X}
      S1 in
      S=X|S1 q(N-1 S1 E)
   end

   fun {DeleteNonBlock q(N S E) X}
      if N>0 then H S1 in
         X=[H] S=H|S1 q(N-1 S1 E)
      else
         X=nil q(N S E)
      end
   end

   fun {DeleteAll q(_ S E) L}
      X in
      L=S E=nil
      q(0 X X)
   end

   fun {Size q(N _ _)} N end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % Correct implementation of monitors
   % Combination of reentrant lock and queue
   % Reentrant lock is split into two operations: get and release
   % Queue is used as wait set for threads: a thread waits by means of a dataflow variable

   % Book version is incorrect!  Code below includes bug fix (see book Errata page)


   proc {NewMonitor ?LockM ?WaitM ?NotifyM ?NotifyAllM}
      Q={NewCell {NewQueue}}
      Token1={NewCell unit}
      Token2={NewCell unit}
      CurThr={NewCell unit}

      % Returns true if got the lock, false if not (already inside)
      fun {GetLock}
         if {Thread.this}\=@CurThr then Old New in
       {Exchange Token1 Old New}
       {Wait Old}
       Token2:=New
       CurThr:={Thread.this}
       true
         else false end
      end

      proc {ReleaseLock}
         CurThr:=unit
         unit=@Token2
      end
   in
      proc {LockM P}
         if {GetLock} then
       try {P} finally {ReleaseLock} end
         else {P} end
      end

      proc {WaitM}
      X in
         Q:={Insert @Q X}
         {ReleaseLock} {Wait X}
         if {GetLock} then skip end
      end

      proc {NotifyM}
      X in
         Q:={DeleteNonBlock @Q X}
         case X of [U] then U=unit else skip end
      end

      proc {NotifyAllM}
      L in
         Q:={DeleteAll @Q L}
         {ForAll L proc {$ X} X=unit end}
      end
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   class Buffer
      attr buf first last n i
         lockm waitm notifyallm

      meth init(N)
         buf:={NewArray 0 N-1 null}
         first:=0 last:=0 i:=0 n:=N
         {NewMonitor @lockm @waitm _ @notifyallm}
      end

      meth put(X)
         {@lockm proc {$}
          if @i==@n then {@waitm} {self put(X)}
          else
             @buf.@last:=X
             last:=(@last+1) mod @n
             i:=@i+1
             {@notifyallm} % notify the waiters
          end
            end}
      end

      meth get(X)
         {@lockm proc {$}
          % while @i==0 then {@waitm} end
          if @i==0 then {@waitm} {self get(X)}
          else
             X=@buf.@first
             first:=(@first+1) mod @n
             i:=@i-1
             {@notifyallm} % notify the waiters
          end
            end}
      end
   end

   B={New Buffer init(3)}

   thread
      {Delay 2000}
      for I in 2..11 do
         {B put(I)}
         {Show 'did put '#I}
      end
   end

   thread
      for I in 1..11 do
         StartTime
      in
         {Show 'will take'#I}
         {Show 'taken'#{B get($)}}
      end
   end

   {Show 'will put 1'}
   {B put(1)}
   {Show 'did put 1'}
end
