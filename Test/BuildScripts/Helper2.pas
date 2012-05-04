unit Helper2;

interface

type 
   TMyHelper2 = helper for Integer
      function ToString : String;
   end;
   
implementation

uses Helper1, Helper3;

function TMyHelper2.ToString : String;
begin
   Result:='Helper2['+IntToStr(Self)+']';
end;
