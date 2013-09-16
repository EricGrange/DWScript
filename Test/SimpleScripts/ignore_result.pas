var i : Integer;
var s : String := '1';
var v : Variant;

IntToStr(i);
StrToInt(s);
StrToFloat(s);
VarToStr(v);

s:='z';

try
   StrToInt(s);
except
   on e : Exception do PrintLn(e.Message.Replace("''", "'"));
end;

try
   StrToFloat(s);
except
   on e : Exception do PrintLn(e.Message);
end;

i:=-1;
try
   Chr(i);
except
   on e : Exception do PrintLn(e.Message);
end;