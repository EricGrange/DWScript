unit InitUnit1;

interface

implementation

procedure StartMe;
begin
   PrintLn('InitUnit1 Start');
end;

procedure EndMe;
begin
   PrintLn('InitUnit1 End');
end;

initialization

StartMe;

finalization

EndMe;

  