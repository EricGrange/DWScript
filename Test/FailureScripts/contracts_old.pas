procedure Test(i : Integer);
begin
ensure
   TObject = old TObject;
   i = old Print('bug');
end;