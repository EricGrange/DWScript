var a : array of Variant;

procedure AsInteger(var v : Variant);
begin
   Print('   AsInteger: ');
   try
      PrintLn(IntToStr(v));
   except
      on E: Exception do
         PrintLn(E.Message);
   end;
end;

procedure AsFloat(var v : Variant);
begin
   Print('   AsFloat: ');
   try
      PrintLn(FloatToStr(v));
   except
      on E: Exception do
         PrintLn(E.Message);
   end;
end;

procedure AsString(var v : Variant);
begin
   Print('   AsString: ');
   try
      PrintLn(LeftStr(v, 9));
   except
      on E: Exception do
         PrintLn(E.Message);
   end;
end;

procedure AsBool(var v : Variant);
begin
   Print('   AsBool: ');
   try
      PrintLn(BoolToStr(v));
   except
      on E: Exception do
         PrintLn(E.Message);
   end;
end;

a.Add(1, 1.5, 'b', True);

for var i := a.Low to a.High do begin
   PrintLn('Index ' +i.ToString);
   AsInteger(a[i]);
   AsFloat(a[i]);
   AsString(a[i]);
   AsBool(a[i]);
end;

try
   AsString(a[-1]);
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   AsString(a[a.Length]);
except
   on E: Exception do
      PrintLn(E.Message);
end;

