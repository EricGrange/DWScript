unit Helper3;

interface

uses Helper1, Helper2;

type 
   TMyHelper3 = helper for Integer
      function ToString : String;
   end;
   
implementation

function TMyHelper3.ToString : String;
begin
   Result:='Helper3{'+IntToStr(Self)+'}';
end;
