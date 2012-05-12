unit Helper3;

interface

uses Helper1, Helper2;

type 
   TMyHelper3 = helper for Integer
      function ToString : String;
      function By3 : Integer;
   end;
   
implementation

function TMyHelper3.ToString : String;
begin
   Result:='Helper3{'+IntToStr(Self)+'}';
end;

function TMyHelper3.By3 : Integer;
begin
   Result:=3*Self;
end;
