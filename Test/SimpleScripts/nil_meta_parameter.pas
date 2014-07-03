procedure Test1(p : TClass);
begin
   if p<>nil then PrintLn('bug');
end;

Test1(nil);

procedure Test2(p : array of TClass);
begin
   if p[0]=nil then PrintLn('ok');
end;

Test2([nil]);
