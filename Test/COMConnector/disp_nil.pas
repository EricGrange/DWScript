var v : ComVariant;

try
   v.method();
except
   on E : Exception do PrintLn(E.Message);
end;

try
   v.prop:='bug';
except
   on E : Exception do PrintLn(E.Message);
end;

try
   Print(v.prop);
except
   on E : Exception do PrintLn(E.Message);
end;