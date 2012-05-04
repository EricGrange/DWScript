type
   TPoint = record
      x, y : Integer;
   end;

type
   TPointHelper = helper for TPoint
      function ToString : String;
      begin
         Result:=IntToStr(x)+','+IntToStr(y);
      end;
   end;
   
type
   TPointArray = array of TPoint;

type
   TPointArrayHelper = helper for TPointArray
      procedure ScaleX(s, cx : Integer);
      begin
         var i : Integer;
         for i:=0 to Self.Length-1 do
            Self[i].X := (Self[i].X - cx) * s + cx;
      end;
   end;

var a : TPointArray;

a.SetLength(2);
a[0].x:=1;
a[1].x:=2;

PrintLn(a[0].ToString);
PrintLn(a[1].ToString);

a.ScaleX(2, 1);

PrintLn(a[0].ToString);
PrintLn(a[1].ToString);
