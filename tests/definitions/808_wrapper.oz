% Secure data storage wrapper
local
   NewWrapper
   Wrap
   Unwrap
   Five
   Six
   R
in


proc {NewWrapper ?Wrap ?Unwrap}
   Key={NewName}
in
   fun {Wrap X} {Chunk.new w(Key:X)} end
   fun {Unwrap W}
      try W.Key
      catch _ then raise error(unwrap(W)) end end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{NewWrapper Wrap Unwrap}
Five={Wrap 5}
Six={Wrap 6}
R={Wrap rec(1 2 3)}
{Show Five}
{Show {Unwrap Five}}
{Show Six}
{Show {Unwrap Six}}
{Show R}
   {Show {Unwrap R}}
end
