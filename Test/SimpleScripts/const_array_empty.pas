var arr := [];
var i := 0;
try
   PrintLn(arr[i]);
except
   on E: Exception do
      PrintLn(E.Message);
end;

