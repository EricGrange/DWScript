type
   TRec = record
      x, y : Integer;
      procedure PrintIt;
      begin
         Print(x); Print(','); PrintLn(y);
      end;
   end;
 
var r : TRec; 
 
procedure IncX(var r : TRec);
begin
   r.X+=1;
end;
 
function Func(y : Integer) : TRec;
begin
   Result.x:=y;
   Result.y:=y*2;
   IncX(Result);
end;
 
function Nested(i : Integer) : TRec;
begin
   if i=0 then Exit(r);
   Result:=Func(i);
   IncX(Result);
end;

IncX(r);
r.PrintIt;

r:=Func(3);
r.PrintIt;

r:=Nested(0);
r.PrintIt;
r:=Nested(1);
r.PrintIt;

var r1:=Nested(0);
IncX(r);
r1.PrintIt;
r.PrintIt;

