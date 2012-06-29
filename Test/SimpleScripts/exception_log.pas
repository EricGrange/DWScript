procedure LogException(Sender: TObject; AException: Exception; Value: string);
begin
   PrintLn(AException.Message);
   PrintLn(Value);
end;

var
   x: Integer;
try
   x := 2012;
   if x > 2000 then
      raise Exception.Create('something went wrong');
except
   on E: Exception do
      LogException(NIL, E, 'woops!');
end;