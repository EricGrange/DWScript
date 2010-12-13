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

var o = TMyObj.Create;

Inc(o);
Inc(o.Field);
Inc(o.GetField);
Inc(o.Direct);
Inc(o.Indirect);

