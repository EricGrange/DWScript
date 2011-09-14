var s : String;
var o : TObject;
var i : IInterface;
var a : array of Variant;

try
   PrintLn(Format('%d', [s])); 
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   PrintLn(Format('%d', [o])); 
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   PrintLn(Format('%d', [i])); 
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   PrintLn(Format('%d', [a])); 
except
   on E: Exception do
      PrintLn(E.Message);
end;
