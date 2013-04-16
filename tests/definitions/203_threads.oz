% Wait instruction, waiting for variable bound in thread.
local
   X Stop
in
   thread {Delay 5000} X=2 end
   thread
      proc {Timer D Init}
         {Delay D}
         if {IsDet Stop} then
            skip
         else
            {Show Init}
            {Timer D Init+1}
         end
      end
   in
      {Timer 1000 1}
   end
   {Wait X} {Show 'got X'} {Show X}
   Stop=unit
end

