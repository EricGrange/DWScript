type
   TBase = class
      procedure Func; virtual;
   end;

type
   TChild = class (TBase)
      procedure Func; reintroduce;
   end;

type
   TSubChild = class (TChild)
      procedure Func;
   end;

procedure TBase.Func;
begin
   PrintLn(ClassName+' here');
end;

procedure TChild.Func;
begin
   PrintLn(ClassName+' now here');
end;

procedure TSubChild.Func;
begin
   PrintLn(ClassName+' finally');
end;

TBase.Create.Func;
TChild.Create.Func;
TSubChild.Create.Func;

var o : TBase;
o:=TBase.Create;
o.Func;
o:=TChild.Create;
o.Func;
o:=TSubChild.Create;
o.Func;
