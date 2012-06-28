var e : Exception;

try
   raise e;
except
   on Ex: Exception do PrintLn(Ex.Message);
end;
