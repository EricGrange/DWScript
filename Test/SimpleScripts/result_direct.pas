function FuncInt(i : Integer) : Integer;
begin
   Result:=i+1;
end;

function FuncFloat(f : Float) : Float;
begin
   Result:=f+1.5;
end;

function FuncString(s : String) : String;
begin
   Result:=s+s;
end;

function FuncBool(b : Boolean) : Boolean;
begin
   Result:=not b;
end;

function FuncObj(o : TObject) : TObject;
begin
   Result:=if Assigned(o) then o else TObject.Create;
end;

type
   TRecord = record
      x : Integer;
   end;      

function FuncRec(i : Integer) : TRecord;
begin
   Result.x:=i;
end;

function FuncRec2 : TRecord;
begin
   Result:=FuncRec(2);
end;

PrintLn(FuncInt(10));
PrintLn(FuncFloat(10));
PrintLn(FuncString('10'));
PrintLn(FuncBool(False));
PrintLn(FuncObj(nil).ClassName);
PrintLn(FuncRec(10).x);
PrintLn(FuncRec2.x);
