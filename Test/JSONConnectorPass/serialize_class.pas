type 
   TPoint = class
      published 
         x, y : Integer;
   end;

type
   TRec = class
      private
         FTopLeft : TPoint;
         
      published
         property TopLeft : TPoint read FTopLeft write FTopLeft;
         BottomRight : TPoint;
         
   end;
   
var p := TPoint.Create;
p.x:=1;
p.y:=2;

var j := JSON.Serialize(p);

PrintLn(j.x);
PrintLn(j.y);
PrintLn(j);


var r := TRec.Create;
r.BottomRight:=p;
r.TopLeft:=new TPoint;
r.TopLeft.x:=3;
r.TopLeft.y:=4;

j := JSON.Serialize(r);
PrintLn(j.TopLeft);
PrintLn(j.BottomRight);
PrintLn(j);