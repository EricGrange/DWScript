type
   TBase = class
      class procedure MyProc;
      class function Virt : String; virtual;
   end;
type
   TChild = class (TBase)
      class function Virt : String; override;
   end;

class procedure TBase.MyProc;
begin
   if Assigned(Self) then begin
      PrintLn(Self.ClassName);
      PrintLn(Self.Virt);
   end else PrintLn('nil');
end;

class function TBase.Virt : String;
begin
   Result:='base_'+Self.ClassType.ClassName;
end;

class function TChild.Virt : String;
begin
   Result:='child_'+Self.ClassType.ClassName;
end;

var o : TBase;
try
   o.MyProc;
except
   on E: Exception do PrintLn(E.Message);
end;
o:=TBase.Create;
o.MyProc;
o:=TChild.Create;
o.MyProc;

TBase.Myproc;
TChild.Myproc;