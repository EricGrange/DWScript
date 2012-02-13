type
   TPoint = record
      X, Y : Integer;
   end;

type
   TTest = class
      p1, p2 : TPoint;
      a1, a2 : array [0..0] of String;
      procedure Test;
      procedure Print;
      procedure Test2;
   end;
   
procedure TTest.Test;
begin
   p1.X:=1;
   p1.Y:=2;
   p2.X:=3;
   p2.Y:=4;
   a1[0]:='a';
   a2[0]:='b';
end;

procedure TTest.Test2;
var
   lp1, lp2 : TPoint;
   la1, la2 : array [0..0] of String;
begin
   lp1.X:=11;
   lp1.Y:=12;
   lp2.X:=13;
   lp2.Y:=14;
   la1[0]:='la';
   la2[0]:='lb';
   p1:=lp1;
   p2:=lp2;
   a1:=la1;
   a2:=la2;
end;
      
procedure TTest.Print;
begin
   Default.Print(p1.X); PrintLn(p1.Y);
   Default.Print(p2.X); PrintLn(p2.Y);
   Default.Print(a1[0]); PrintLn(a2[0])
end;

var t:=TTest.Create;
t.Print;
t.Test;
t.Print;
t.Test2;
t.Print;