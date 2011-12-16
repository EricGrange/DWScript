type
   TMyObj = class
      Field : Integer;
      function GetField : Integer;
      property Direct : Integer read Field write Field;
      property DirectRead : Integer read Field;
      property Indirect : Integer read GetField write Field;
   end;

function TMyObj.GetField : Integer;
begin
   Result:=Field;
end;

procedure VarTest(var v : Integer);
begin
end;

var o = TMyObj.Create;

VarTest(o);
VarTest(o.Field);
VarTest(o.GetField);
VarTest(o.Direct);
VarTest(o.Indirect);

