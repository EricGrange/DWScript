unit HelperUser;

uses HelperLib;

begin
   const Hello = TMyArray.Hello;
end;

type 
   TMyUser = class
      public
         class const Hello = TMyArray.Hello;
   end;
   

