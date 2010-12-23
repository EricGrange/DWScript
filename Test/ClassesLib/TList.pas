type TMyObj = class
      Field : String;
      constructor Create(v : String);
   end;

constructor TMyObj.Create(v : String);
begin
   Field:=v;
end;

procedure ListObjFields(list : TList);
var 
   i : Integer;
begin
   for i:=0 to list.Count-1 do 
      PrintLn((list[i] as TMyObj).Field);
end;

var list := TList.Create;

list += TMyObj.Create('alpha');
var beta = TMyObj.Create('beta');
list += beta;
list += TMyObj.Create('omega');

ListObjFields(list);
list -= beta;
ListObjFields(list);