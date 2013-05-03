% List with unbounded tail filled at one second interval
local
   L Fill Print
   StartTime
in
   fun {Fill V}
      {Delay 1000}
      if V>3 then
	      nil
      else
         V+1|{Fill V+1}
      end
   end

   proc {Print L}
      case L
      of X|Xs then
    {Show ({Time.time}-StartTime)}
	 {Show '*'#X#'*'}
	 {Print Xs}
      [] nil then
	 skip
      end
   end
   StartTime={Time.time}
   {Print thread {Fill 0} end }
end

