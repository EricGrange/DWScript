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
         List : array of TPoint;
         
   end;
   
var p : TPoint;

PrintLn(JSON.PrettyStringify(p, ''));

p := TPoint.Create;

p.x:=1;
p.y:=2;

PrintLn(JSON.PrettyStringify(p, '-'));

var r := TRec.Create;
r.BottomRight:=p;
r.TopLeft:=new TPoint;
r.TopLeft.x:=3;
r.TopLeft.y:=4;
r.List.Add(new TPoint);

PrintLn(JSON.PrettyStringify(r, '   '));