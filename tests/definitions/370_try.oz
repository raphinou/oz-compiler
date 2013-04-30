% Simple try catch statement
try raise testException end
   catch otherException then {Show other}
   [] testException then {Show whatWeWant}
end

