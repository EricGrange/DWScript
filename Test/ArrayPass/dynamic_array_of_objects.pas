type TMyObj = class
      Field : Integer;
      constructor Create(i : Integer);
   end;

type TMyObjects = array of TMyObj;

constructor TMyObj.Create(i : Integer);
begin
   Field:=i*2;
end;

var objs : TMyObjects;
objs:=new TMyObj[3];

var i : Integer;

for i:=0 to Length(objs)-1 do
   objs[i]:=new TMyObj(i+1);

for i:=Low(objs) to High(objs) do
   PrintLn(objs[i].Field);

objs.Add(TMyObj.Create(10));
objs.Delete(1, 2);

for i:=Low(objs) to High(objs) do
   PrintLn(objs[i].Field);
