type 
   TRec = record
      Left, Right : Integer;
      procedure PrintMe;
      begin
         Print(Left);
         PrintLn(Right);
      end;
   end;

procedure DoIt(r1, r2 : TRec);
begin
   r1.PrintMe;
   r2.PrintMe;
end;

procedure StartIt(r1, r2 : TRec);
var
   ra : TRec;
   rb : TRec;
begin
   ra:=r1;
   rb:=r2;
   DoIt(ra, rb);
   DoIt(rb, ra);
end;

var r1, r2 : TRec;

r1.Left:=1;
r1.Right:=2;
r2.Left:=3;
r2.Right:=4;

StartIt(r1, r2);
StartIt(r2, r1);