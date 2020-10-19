type TTest = record
   A : array of String;
end;

procedure proc;
begin
   var t : TTest;
   t.A.Add('CF');
   PrintLn(t.A.Join(','));
end;

proc;
proc;