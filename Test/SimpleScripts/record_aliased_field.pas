type
   TMyInt1 = Integer;
   
type
   TMyInt2 = TMyInt1;
   
type
   TOther = record
      i : TMyInt2;
   end;
   
type 
   TSub = TOther;

type 
   TPoint = record
      x : TMyInt1;
      y : TSub;
      procedure P;
      begin 
         Print(x); 
         Print(', ');
         PrintLn(y.i);
      end;
   end;

var p, p2 : TPoint;
p2.x:=1;
p2.y.i:=2;

p.P;

p:=p2;
p.P;

p.x:=p.y.i;
p.P;



