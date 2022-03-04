type
   TRec = record
      F1 :  Integer;
      F2 : String;
   end;

procedure PrintArray(a : array of TRec);
var
   i : Integer;
begin
   Print(IntToStr(Length(a))+':{');
   for i:=0 to High(a) do begin
      if i>0 then
         Print(',');
      Print('(');
      Print(IntToStr(a[i].F1));
      Print(',');
      Print(a[i].F2);
      Print(')');
   end;
   PrintLn('}');
end;
   
var i : Integer;
var a : array of TRec;

PrintArray(a);

a.SetLength(3);
PrintArray(a);

a[0].F1:=1;
a[0].F2:='one';
a[1].F1:=2;
a[1].F2:='two';
a[2].F1:=3;
a[2].F2:='three';
PrintArray(a);

a.SetLength(3);
PrintArray(a);

a.SetLength(2);
PrintArray(a);

a.SetLength(3);
PrintArray(a);

a.SetLength(1);
PrintArray(a);

a.Clear;
PrintArray(a);

a.SetLength(3);
PrintArray(a);
