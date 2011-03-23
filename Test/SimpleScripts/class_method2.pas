type
   TClassB = class
      class procedure ClassProc;
      method DoIt;
   end;

class procedure TClassB.ClassProc;
begin
   PrintLn(ClassName+' ClassProc');
end;

method TClassB.DoIt;
begin
   ClassProc;
   Self.ClassProc;
end;

var b := TClassB.Create;

b.ClassProc;
TClassB.ClassProc;
b.DoIt;