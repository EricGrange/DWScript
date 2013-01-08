type 
   TPoint = record
      published 
         x, y : Integer;
   end;

type
   TRec = record
      private
         FTopLeft : TPoint;
         
      published
         property TopLeft : TPoint read FTopLeft write FTopLeft;
         BottomRight : TPoint;
         
   end;
   
var p : TPoint = (x: 1; y: 2);

PrintLn(JSON.Stringify(p));

var r : TRec;
r.BottomRight:=p;
p.y:=3;
r.TopLeft:=p;

PrintLn(JSON.Stringify(r));