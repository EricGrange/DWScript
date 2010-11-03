function GetObj : TObject;
begin
   Result:=TObject.Create;
end;

procedure UseObj;
var
   o : TObject;
begin
   o:=GetObj;
   GetObj;
end;

var i : Integer;
for i:=1 to 10 do
   GetObj;
for i:=1 to 10 do
   UseObj;