% Inheritance and features
% From http://www.mozart-oz.org/home/doc/tutorial/node10.html#label56
local
   ApartmentC AptC MyAptC1 Apt1 Apt2 Apt3 Apt4
in
   class ApartmentC from BaseObject
      meth init skip end
   end
   class AptC from ApartmentC
      feat
         streetName: york
         streetNumber:100
         wallColor:white
         floorSurface:wood
   end
   class MyAptC1 from ApartmentC
      feat streetName
   end
   Apt1 = {New AptC init}
   Apt2 = {New AptC init}
   {Show Apt1.streetName}
   {Show Apt2.streetName}

   Apt3 = {New MyAptC1 init}
   Apt4 = {New MyAptC1 init}
   Apt3.streetName = kungsgatan
   Apt4.streetName = sturegatan

   {Show Apt3.streetName}
   {Show Apt4.streetName}
end

