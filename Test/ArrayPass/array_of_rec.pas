type
   TRec = record
      Field : Integer
   end;
   
procedure PrintArray(a : array of TRec);
var
   i : Integer;
begin
   Print(IntToStr(Length(a))+':{');
   for i:=0 to High(a) do begin
      if i>0 then
         Print(',');
      Print(IntToStr(a[i].Field));
   end;
   PrintLn('}');
end;
      
var r : TRec;
var a : array of TRec;

a.Add(r);
r.Field:=1;
a.Add(r);
r.Field:=-1;
a.Insert(0, r);

PrintArray(a);

r.Field:=123;

PrintArray(a);

a.Delete(1);

PrintArray(a);
     