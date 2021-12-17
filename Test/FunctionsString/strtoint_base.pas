for var i := 2 to 36 do begin
   Print(StrToInt('101', i));
   var v := 0;
   if TryStrToInt('00101', i, v) then
      PrintLn('  ' + v.ToString)
   else PrintLn('  booh');
end;

var base := 10;  // use a varable to force runtime evaluation

try
   PrintLn(StrToInt('', base));
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn(StrToInt('1', base div base));
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn(StrToInt('&', base));
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn(StrToInt('y', base+25));
   PrintLn(StrToInt('z', base+25));
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn(StrToInt('+7fffffffffffffff', base+6));
   PrintLn(StrToInt('-7fffffffffffffff', base+6));
   PrintLn(StrToInt('zzzZzzzZzzzzzz', base+26));
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn(StrToInt('4444444444444444', base+6));
   PrintLn(StrToInt('44444444444444444', base+6));
except
   on E: Exception do PrintLn(E.Message);
end;
