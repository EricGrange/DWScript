var v : ComVariant;

try
   v.method();
except
   on E : Exception do PrintLn(E.Message);
end;