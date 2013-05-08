% Declarative memoization in Oz

% Author: Lyle Kopnicky
% Date: Jan. 20, 2010

% In Chapter 4 of CTM, the authors claim that it is not possible to
% memoize a function declaratively without changing the interface to
% thread state around. I will now show you how it can be done.  I've
% already shown this to Prof. Van Roy, and he acknowledges that my
% solution works, but that it is not as efficient in memory usage as
% the stateful version. Still, I thought you folks might find it of
% interest.

% It is worth stating that I was inspired by techniques I had learned
% in Haskell, a lazy functional language. Here's some info on the
% Haskell Wiki: http://www.haskell.org/haskellwiki/Memoization
% Here's an article I just found that goes even further with the
% idea of using a trie to memoize functions:
% http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries/

% Let's start with a function that has rather poor performance: the
% Fibonacci function. Here is the original, unmemoized Fib function:

local
   WaitList
   NaiveFib
   LMap
   LEnumFrom
   MemoizeInList
   OpenFib
   Fix
   MemoFix
   Double
   MemoizeInLazyList
   NaiveFibMemoizedInList
   R
   MemoizeInTrie
   MemoizeORFInTrie
in






   fun {WaitList L}
      fun {WaitListInt L Lf}
         case Lf
         of X|Xs then
            {Wait X}
            {WaitListInt L Xs}
         [] nil then
       L
         end
      end
   in
      {WaitListInt L L}
   end
fun {NaiveFib N}
   case N
   of 0 then 1
   [] 1 then 1
   [] N then {NaiveFib N-2} + {NaiveFib N-1}
   end
end

% To memoize this, we can create a lazy list in an outer context of
% the function, that enumerates every value of the function. First,
% we'll need a lazy version of Map:

fun lazy {LMap Xs F}
   case Xs
   of nil then nil
   [] X|Xr then {F X}|{LMap Xr F}
   end
end

% And a lazy version of List.number, that generates an infinite
% stream of natural numbers:

fun lazy {LEnumFrom N}
   N|{LEnumFrom N+1}
end

% Here is the memoization function:

fun {MemoizeInList F}
   Cache = {LMap {LEnumFrom 0} F} in
   fun {$ N} {List.nth Cache N+1} end
end

% You can now create a memoized version of Fib, and test it:

NaiveFibMemoizedInList = {MemoizeInList NaiveFib}
{Show inlist#{WaitList {Map {List.number 0 30 1} NaiveFibMemoizedInList}}}

% It may take a little while to calculate {NaiveFibMemoizedInList 35}
% the first time, but try it again and it's instant. Unfortunately,
% the recursive calls within NaiveFib don't use the memoized version.
% If they could, we could speed up the function from exponential time,
% or O(e^n), to quadratic time, or O(n^2). Quadratic because it still
% takes O(n) time to look up a value in the cache.

% We can accomplish this by rewriting Fib in an "open recursive"
% fashion. That is, it doesn't call itself recursively. Instead, it
% takes a function as an argument, and we replace the recursive calls
% with calls to that function:

fun {OpenFib Fib N}
    case N
    of 0 then 1
    [] 1 then 1
    [] N then {Fib N-2} + {Fib N-1}
    end
end

% This function only handles a single step of the Fibonacci operation.
% It takes an external function to "tie the recursive knot". Such a
% function is called a fixed-point combinator. The simplest one
% transforms OpenFib into NaiveFib:

fun {Fix F}
   fun {$ X} {F {Fix F} X} end
end

% So {Fix OpenFib} is equivalent to NaiveFib. Now we can write a
% fixed-point operator that implements memoization:

fun {MemoFix F}
   Cache MemoFixedF in
   Cache = {LMap {LEnumFrom 0} fun {$ N} {F MemoFixedF N} end}
   fun {MemoFixedF N} {List.nth Cache N+1} end
   MemoFixedF
end

R={{MemoFix OpenFib} 100}
{Wait R}
{Show memofix#R}
% Now you can call {{MemoFix OpenFib} 100} and get a result very
% quickly. The recursive calls now all use the memoized version of
% OpenFib.

% "But wait," you object, "didn't CTM claim you couldn't do
% declarative memoization without changing the interface of the
% function you're memoizing? You've changed the interface." Correct.
% But MemoizeInList doesn't. I just wanted to show you how, with a
% small modification, you could get vastly better recursive
% performance.

% These memoizer functions work well for the fibonacci function, which
% recursively calls itself on every smaller value. But if you memoize
% a function that doesn't call itself on smaller values, or only on
% some of them, you pay a horrible performance penalty. Take, for
% example, this function for doubling a number:

fun {Double N} {Delay 1000} N*2 end

% I introduced the artificial delay to simulate a lengthy calculation.
% If you assign {MemoizeInList Double} to a variable, then call it on
% the value 9, you will have a delay of 10 seconds before you get your
% answer. If you subsequently call it on the input value 8, you'll get
% your answer back immediately. That's because, the cache, which is a
% list, had to be evaluated for every value below 9 before it got to
% evaluating it for 9. To speed up evaluation of these types of
% functions, we can use a bit more laziness:

fun {MemoizeInLazyList F}
   LF Cache in
   Cache = {LMap {LEnumFrom 0} LF}
   fun lazy {LF N} {F N} end
   fun {$ N} V in V={List.nth Cache N+1} {Wait V} V end
end
{Show naive#{NaiveFib 32}}
{Show lazylist#{{MemoizeInLazyList NaiveFib} 32}}

% By caching the evaluation of LF, a lazy version of F, instead of F
% itself, the cache can skip over the calculation of input values
% smaller than the one on which we're calling the function. The Wait
% call forces evaluation on the input value we care about, so that
% the memoized function is not itself lazy.

% We can do better on the asymptotic performance of memoization. It
% still takes O(n) time to look up the result for an input value n.
% Instead of arranging the cache as a list, we can organize it as a
% trie, or prefix tree. Imagine the input value as a binary number.
% Then the path through the trie will be determined by the bits in the
% representation. If we can evaluate each step along the path in
% constant time, then lookup time will be reduced to O(log n) for an
% input value n.

% We can either follow the bits of the value from left to right, i.e.,
% most significant to least significant, or right to left. If left to
% right, we can ignore the leftmost bit (which is always one), and go
% left for a zero bit and right for a one bit. We then get a trie
% which looks like this at the top (in decimal and in binary):

%     1             1
%   2   3      10      11
%  4 5 6 7   100 101 110 111

% We've excluded the number zero, but we can make up for this by
% adding one to the input value before looking it up in the cache.
% One other problem with going from left to right is that it's easier
% to recurse starting from the least significant bit: you can use the
% modulus function. If we go from right to left instead, we get a trie
% that looks like this:

%         0                          '
%     0       1              0               1
%   0   2   1   3       00      10      01      11
%  0 4 2 6 1 5 3 7    000 100 010 110 001 101 011 111

% Here I've used a single tick (') to represent the empty string, that
% evaluates to zero. The odd thing about this trie is that the same
% value appears infinitely many times. But we won't waste time
% generating all of those. The topmost level on which a value appears
% is the only place in which it will actually get evaluated. Here's a
% function, MemoizeInTrie, that looks a lot like MemoizeInLazyList,
% but uses the right-to-left binary trie structure instead of a list:

local LEnum Lookup in
   fun lazy {LEnum N Pow2 F}
      cache(val:{F N} left:{LEnum N Pow2*2 F} right:{LEnum N+Pow2 Pow2*2 F})
   end
   fun {Lookup Cache N}
      Value Left Right in
      Cache = cache(val:Value left:Left right:Right)
      if N == 0
      then Value
      else case N mod 2
  of 0 then {Lookup Left  N div 2}
  [] 1 then {Lookup Right N div 2}
  end
      end
   end
   fun {MemoizeInTrie F}
      LF Cache in
      fun lazy {LF N} {F N} end
      Cache = {LEnum 0 1 LF}
      fun {$ N} V in V={Lookup Cache N} {Wait V} V end
   end
end

{Show trie#{{MemoizeInTrie NaiveFib} 32}}
% Whether the asymptotic storage or lookup time for a single value is
% O(log n) depends on how long arithmetic operations take. We're
% executing a modulus, a division, and a multiplication by two in
% various places. If the binary representation were a list of bits,
% with the least significant bit at the head, each of these operations
% would take constant time. In reality, they're probably represented
% in Oz by arithmetic instructions in the CPU, for numbers with few
% enough bits. For larger numbers, they probably aren't constant time,
% but since the code could in theory use the list representation,
% I'll stand by my asymptotic limit.

% Finally, here's the same trie memoization, but for open recursive
% functions, like OpenFib:

local LEnum Lookup in
   fun lazy {LEnum N Pow2 F}
      cache(val:{F N} left:{LEnum N Pow2*2 F} right:{LEnum N+Pow2 Pow2*2 F})
   end
   fun {Lookup Cache N}
      Value Left Right in
      Cache = cache(val:Value left:Left right:Right)
      if N == 0
      then Value
      else case N mod 2
  of 0 then {Lookup Left  N div 2}
  [] 1 then {Lookup Right N div 2}
  end
      end
   end
   fun {MemoizeORFInTrie ORF}
      LF MF Cache in
      fun lazy {LF N} {ORF MF N} end
      fun {MF N} V in V={Lookup Cache N} {Wait V} V end
      Cache = {LEnum 0 1 LF}
      MF
   end
end

{Show orf#{{MemoizeORFInTrie OpenFib} 32}}


end
% There are two more generalizations we can make to enhance the
% usefulness of declarative memoization. One is to permit values other
% than natural numbers as input arguments. This can be addressed by
% serializing the values using Pickle.pack. The other is to allow more
% than one argument. This can be achieved by combining the arguments
% into a list or record structure, then serializing it with
% Pickle.pack. The pack function may not yield the most efficient bit
% strings for any particular data structure, but you could do as Conal
% Elliott does in the aforementioned article and develop specialized
% functions to turn certain datatypes into tries.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% One thing I did not address in my previous e-mail was the memory
% usage of the implementations. It is clear that the very heart of
% memoization involves a tradeoff between calculation time and memory
% use. The efficiency of memory use depends on the pattern of usage.

% The functions memoizing results in a list use O(n) memory to store a
% single result for input value n. To store m values n_1...n_m takes
% not O(n*m) space, however, but max(n_1, ..., n_m). The stateful
% approach, using, say, a hash table, takes O(m) space to store m
% values, which is less than max(n_1, ..., n_m), unless n_m = m, in
% which case they are equal. It turns out in the case of the Fibonacci% function that, since all input values less than the highest input
% value must be processed in order to compute the result, so it is no
% less (asymptotically) memory-efficient than the stateful version.
% The memoizing list has some overhead, but so does a hash table.

% The trie approach takes O(log n) space to store a single result.
% For m values n_1...n_m it takes close to O(log n_1 + ... + log n_m)
% for very sparse values, up to a limit of O(n_m) when all the slots
% for the smaller values are filled in.

% But, let us not forget that the stateful approach has the chance to
% free the memory once it's no longer needed. The declarative approach
% can allow the memory to be garbage-collected as soon as the
% references to the memoized function can be dropped. But the stateful
% version can choose to just memoize, say, the results for the last
% 100 distinct input values.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

