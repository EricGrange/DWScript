{
Demo: Object Orientated Programming (OOP)

}
type
  AClass = class
    function Q: string;
  end;

type
  BClass = class(AClass)
    function Q: string;
  end;

type
  CClass = class
    function Q: string;
  end;

function AClass.Q: string;
begin
  Result := 'AClass.Q: Static method';
end;

function BClass.Q: string;
begin
  Result := 'BClass.Q: Static method';
end;

function CClass.Q: string;
begin
  Result := 'CClass.Q: Static method';
end;

var objA, objB : AClass;

objA := AClass.Create;
objB := BClass.Create;

if objA is AClass then PrintLn('objA is AClass');
if objA is BClass then PrintLn('objA is BClass');
if objA is CClass then PrintLn('objA is CClass');

if objB is AClass then PrintLn('objB is AClass');
if objB is BClass then PrintLn('objB is BClass');
if objB is CClass then PrintLn('objB is CClass');

PrintLn((objA as AClass).Q);
try
   PrintLn((objA as BClass).Q);
except
   PrintLn('objA isn''t BClass');
end;
try
   PrintLn((objA as CClass).Q);
except
   PrintLn('objA isn''t CClass');
end;

PrintLn((objB as AClass).Q);
PrintLn((objB as BClass).Q);
try
   PrintLn((objB as CClass).Q);
except
   PrintLn('objB isn''t CClass');
end;

