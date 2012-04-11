type
   TRec = record
      Field : Integer;
   end;
   
function Func1(const r : TRec) : Integer;
begin
   Result:=r.Field;
end;

function Func2(const r : TRec) : TRec;
begin
   Result.Field:=r.Field+1;
end;

var r : TRec;
PrintLn(Func1(r));
PrintLn(Func1(Func2(r)));
r:=Func2(r);
PrintLn(Func1(r));
PrintLn(Func1(Func2(r)));
