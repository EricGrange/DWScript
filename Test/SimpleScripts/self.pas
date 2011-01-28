type
   TBase = class
      Field : Integer;
      procedure MyProc;
      function Virt : Integer; virtual;
   end;
type
   TChild = class (TBase)
      function Virt : Integer; override;
      property Prop : Integer read Field;
   end;

procedure TBase.MyProc;
begin
   if Assigned(Self) then begin
      PrintLn(Self.ClassName);
      PrintLn(Self.Virt);
   end else PrintLn('nil');
end;

function TBase.Virt : Integer;
begin
   Result:=Self.Field;
end;

function TChild.Virt : Integer;
begin
   Result:=Self.Prop*100;
end;

var o : TBase;
o.MyProc;
try
   o.Virt;
except
   on E: Exception do PrintLn(E.Message);
end;
o:=TBase.Create;
o.Field:=1;
o.MyProc;
o:=TChild.Create;
o.Field:=2;
o.MyProc;