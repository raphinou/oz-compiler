% Locks
local
   L
   C
in
   L={NewLock}
   C={NewCell 0}
   thread
      {Delay 500}
      lock L then
	 {Show 't1 got lock'}
	 C:=@C+1
	 {Show @C}
      end
   end
   thread
      {Delay 500}
      lock L then
	 {Show 't2 got lock'}
	 C:=@C+2
	 {Show @C}
      end
   end
   thread
      {Delay 700}
      {Show 't1 and t2 try to get lock, but it is not available'}
   end
   thread
      lock L then
	 {Show 't3 got lock and waits 1s'}
	 {Delay 1000}
	 C:=@C+3
	 {Show @C}
      end
   end
end
