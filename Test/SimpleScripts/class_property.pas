type
   TBase = class
      class function GetName : String; virtual;
      class procedure SetName(v : String); virtual;
      property Name : String read GetName write SetName;
   end;

type
   TChild = class (TBase)
      class function GetName : String; override;
      class procedure SetName(v : String); override;
   end;

type
   TSubChild = class (TChild)
      class procedure SetName(v : String); override;
   end;

class function TBase.GetName : String;
begin
   Result:='Base';
end;

class procedure TBase.SetName(v : String);
begin
   PrintLn('TBase.SetName on '+Name+' with '+v);
end;

class function TChild.GetName : String;
begin
   Result:='Child';
end;

class procedure TChild.SetName(v : String);
begin
   PrintLn('TChild.SetName on '+Name+' with '+v);
end;

class procedure TSubChild.SetName(v : String);
begin
   PrintLn('TSubChild.SetName on '+Name+' with '+v);
end;

PrintLn('Direct');
TBase.Name:=TBase.Name;
TChild.Name:=TChild.Name;
TSubChild.Name:=TSubChild.Name;

PrintLn('Class Var');
var v : class of TBase;
v:=TBase;
v.Name:=v.Name;
v:=TChild;
v.Name:=v.Name;
v:=TSubChild;
v.Name:=v.Name;

PrintLn('Object Var');
var o : TBase;
o:=TBase.Create;
o.Name:=o.Name;
o:=TChild.Create;
o.Name:=o.Name;
o:=TSubChild.Create;
o.Name:=o.Name;

o:=nil;
try
   o.Name:=o.Name;
except
   on E: Exception do
      PrintLn(e.Message);
end;

