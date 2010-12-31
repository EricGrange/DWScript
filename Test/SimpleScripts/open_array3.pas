procedure sss(const arr: array of const);
begin
   Print(Length(arr));
   Print(': ');
   var i : Integer;
   for i:=0 to Length(arr)-1 do begin
      Print(arr[i]);
      Print(',');
   end;
   PrintLn('');
end;

procedure sss2(const arr: array of const);
begin
  sss(arr);
end;

sss2([]);
sss2([1]);
sss2([2,3]);
sss2(['four','five','six']);
