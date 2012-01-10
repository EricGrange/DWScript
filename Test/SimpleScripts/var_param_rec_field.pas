type
   TRec = record
      a : Integer;
   end;

procedure Test(var a : Integer);
begin
   Inc(a);
end;

var r : TRec;

Test(r.a);
PrintLn(r.a);

Test(r.a);
PrintLn(r.a);