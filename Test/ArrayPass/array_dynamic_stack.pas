type TRec = record
   V : Integer;
   N : String;
   procedure Print; begin PrintLn(IntToStr(v)+', '+N); end;
end;

var a : array of Integer;
var r : array of TRec;
var i : Integer;

for i:=1 to 10 do begin
   var buf : TRec;
   buf.V:=i*2;
   buf.N:=UpperCase(IntToHex(buf.V, 2));
   r.Push(buf);
   a.Push(i);
end;

procedure PopAFew(n : Integer; a : array of Integer; r : array of TRec);
begin
   while n>0 do begin
      Print(a.Pop);
      Print(': ');
      r.Pop.Print;
      Dec(n);
   end;
end;

PopAFew(5, a, r);
PrintLn('Remain: '+IntToStr(Length(a))+', '+IntToStr(Length(r)));

i:=a.Pop;
var buf := r.Pop;

PrintLn('Remain: '+IntToStr(Length(a))+', '+IntToStr(Length(r)));

PopAFew(4, a, r);

PrintLn('Remain: '+IntToStr(Length(a))+', '+IntToStr(Length(r)));
