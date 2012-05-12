unit Helper2;

interface

uses Helper1;

type 
   TMyHelper2 = helper for Integer
      function ToString : String;
   end;
   
function H2Use1(i : Integer) : Integer;
function H2Use3(i : Integer) : Integer;
   
implementation

uses Helper3;

function TMyHelper2.ToString : String;
begin
   Result:='Helper2['+IntToStr(Self)+']';
end;

function H2Use1(i : Integer) : Integer;
begin
   Result:=i.By2;
end;

function H2Use3(i : Integer) : Integer;
begin
   Result:=i.By3;
end;