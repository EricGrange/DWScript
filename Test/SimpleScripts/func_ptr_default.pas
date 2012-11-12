type
	TStringProc = procedure (s : String);

procedure Test(s : String; f : TStringProc = nil);
begin
   if Assigned(f) then
      f(s)
   else PrintLn(s);
end;

procedure PrintWrap(s : String);
begin
   Print('wrap: ');
   PrintLn(s);
end;

Test('Hello');
Test('World', nil);
Test('Byebye', PrintWrap);
Test('World', @PrintWrap);

var p := @PrintWrap;

Test('Hello', p);
p := nil;
Test('Again', p);