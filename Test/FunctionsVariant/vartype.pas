if VarType('str')<>varString then PrintLn('varString bug');
if VarType(123)<>varInt64 then PrintLn('varInt64 bug');
if VarType(12.3)<>varDouble then PrintLn('varDouble bug');
if VarType(True)<>varBoolean then PrintLn('varBoolean bug');

