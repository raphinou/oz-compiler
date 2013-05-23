% Variations on barrier synchronization

 % By Peter Van Roy, Dec. 3, 2008

 % This file gives some more examples of functional
 % building blocks as concurrency patterns.  The examples
 % also show when we can stay in the declarative
 % concurrent model and when we need to use ports.

 % Barrier synchronization is a basic operation in
 % concurrent and parallel computing.  We start n tasks
 % concurrently and we synchronize on the completion of
 % all tasks.

 % The procedure Barrier takes a list of zero-argument
 % procedures, representing tasks, executes them
 % concurrently and terminates when all have terminated.
local
   Barrier
   PartialBarrier
in

proc {Barrier Ps}
   Xs={Map Ps fun {$ P} X in thread {P} X=ok end X end}
in
   for X in Xs do {Wait X} end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Partial barrier synchronization

% This is a variation on barrier synchronization.
% The procedure PartialBarrier takes an extra integer
% argument n.  It terminates when at least n procedures
% have terminated.

% This operation is useful in component deployment.
% For example, when deploying n services it can be
% useful to provide the service as soon as some small
% number (e.g., 1 or half) of the services is up and running.

% Note that Barrier is written completely in the declarative
% concurrent model, whereas PartialBarrier uses a port.
% This is quite normal since PartialBarrier can exhibit
% nondeterministic behavior whereas Barrier is completely
% deterministic.

% I learned of this operation from the thesis defense
% of Christophe Taton at INRIA Rhonalpes on Dec. 2, 2008.

proc {PartialBarrier Ps N}
   S Pt {NewPort S Pt}
in
   for P in Ps do thread {P} {Send Pt 1} end end
   {Nth S N _} % Wait until S has at least N members
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example comparing barrier and partial barrier

% This displays 'peekaboo' right after displaying 'a'.
{Show start}
{PartialBarrier
 [proc {$} {Delay 1000} {Show a} end
  proc {$} {Delay 3000} {Show b} end
  proc {$} {Delay 2000} {Show c} end] 1}
{Show peekaboo}

% This displays 'peekaboo' after 'a', 'b', and 'c'
{Show start}
{Barrier
 [proc {$} {Delay 1000} {Show a} end
  proc {$} {Delay 3000} {Show b} end
  proc {$} {Delay 2000} {Show c} end]}
{Show peekaboo}

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


