type
   TBase = class end;
type
   TChild = class (TBase);
type
   TSubChild = class (TChild);
type
   TOtherChild = class(TBase);
   
procedure PrintHierarchy(base : TClass);
begin
   while Assigned(base) do begin
      Print(' => ');
      Print(base.ClassName);
      base:=base.ClassParent;
   end;
   PrintLn('');
end;

PrintHierarchy(TBase);
PrintHierarchy(TChild);
PrintHierarchy(TSubChild);
PrintHierarchy(TOtherChild);

