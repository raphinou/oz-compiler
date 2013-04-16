% Quicksort from the book CTMCP, lazy and eager versions
local
   LAppend
   Partition
   LQuicksort
   Xs
   EAppend
   EQuicksort
   Xs
in
   fun lazy {LAppend Xs Ys}
      case Xs of X|Xr then
         X|{LAppend Xr Ys}
      [] nil then Ys
      end
   end

   % Partition is not lazy; it does not have to be.
   proc {Partition L2 X ?L ?R}
      case L2
      of Y|M2 then
         if Y<X then Ln in
      L=Y|Ln
      {Partition M2 X Ln R}
         else Rn in
      R=Y|Rn
      {Partition M2 X L Rn}
         end
      [] nil then L=nil R=nil
      end
   end

   fun lazy {LQuicksort L}
      % Use first element of L as pivot
      case L of X|L2 then Left Right SL SR in
         {Partition L2 X Left Right}
         SL={LQuicksort Left}
	 SR={LQuicksort Right}
	 {LAppend SL X|SR}
      [] nil then nil end
   end

   % Example execution:
   Xs={LQuicksort [5 8 4 7 3 2 7 6 0 5 4 1 0 4]}
   {Show Xs}

   {Show Xs.1}
   {Show Xs.2.1}
   {Show Xs}

   % This will calculate the first three smallest elements
   % and display the third smallest element:
   {Show Xs.2.2.1}

   {Show Xs.2.2.2.1}
   {ForAll Xs Show}
   {Show Xs}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nonlazy (eager) version of quicksort
% Same definitions except not annotated 'lazy'

   fun {EAppend Xs Ys}
      case Xs of X|Xr then
         X|{EAppend Xr Ys}
      [] nil then Ys
      end
   end


   fun {EQuicksort L}
      % Use first element of L as pivot
      case L of X|L2 then Left Right SL SR in
         {Partition L2 X Left Right}
         SL={EQuicksort Left}
         SR={EQuicksort Right}
         {EAppend SL X|SR}
      [] nil then nil end
   end

   % Example execution:
   Xs={EQuicksort [5 8 4 7 3 2 7 6 0 5 4 1 0 4]}
   {Show Xs}
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

