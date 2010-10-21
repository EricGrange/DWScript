var arr := [];
try
   PrintLn(arr[0]);
except
   on E: Exception do
      PrintLn(E.Message);
end;

