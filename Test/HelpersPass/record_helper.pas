type
   TRec = record
      x, y : Integer;
   end;
   
type
   TRecHelper = helper for TRec
      const OneTwo : TRec = (x:1; y:2);
      function Next : TRec;
   end;

function TRecHelper.Next : TRec;
begin
   Result.x:=Self.x+1;
   Result.Y:=y+2;
end;

var r := TRec.OneTwo.Next;

PrintLn(IntToStr(r.x)+', '+IntToStr(r.y));

r:=r.OneTwo;

PrintLn(IntToStr(r.x)+', '+IntToStr(r.y));

r:=r.Next;

PrintLn(IntToStr(r.x)+', '+IntToStr(r.y));