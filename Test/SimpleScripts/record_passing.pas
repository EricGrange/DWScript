type 
   TPoint = record
      x, y : Integer;
   end;

procedure PrintPoint(const xy : TPoint);
begin
   Print(xy.x);
   Print(', ');
   PrintLn(xy.y);
end;

procedure PassCopy(ab : TPoint);
begin
   ab.x:=1;
   PrintPoint(ab);
end;

procedure PassVar(var ab : TPoint);
begin
   ab.y:=2;
   PrintPoint(ab);
end;

var xy : TPoint;

PrintPoint(xy);
PassCopy(xy);
PassVar(xy);
PrintPoint(xy);