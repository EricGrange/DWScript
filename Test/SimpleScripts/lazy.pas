var i, k : Integer;

function PostInc(var i : Integer) : Integer;
begin
   Result:=i;
   i:=i+1;
end;

function GlobInc : Integer;
begin
   Result:=k;
   k:=k+5;
end;

function CondEval(eval : Boolean; lazy a : Integer) : Integer;
begin
   if eval then
      Result:=a
   else Result:=0;
end;

procedure LazyPrint(lazy a : Integer);
var
   i : Integer;
begin
   for i:=1 to 10 do begin
      Print(a);
      Print(',');
   end;
   PrintLn('');
end;

PrintLn('Pre-Inc');
k:=10;
LazyPrint(Inc(k));
k:=5;
LazyPrint(Inc(k, 5));

PrintLn('Post-Inc');
k:=10;
LazyPrint(PostInc(k));
k:=5;
LazyPrint(GlobInc);

PrintLn('Conditional eval');
for k:=-1 to 1 do
   PrintLn(CondEval(k<>0, 1 div k));


