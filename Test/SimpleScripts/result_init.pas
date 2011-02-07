function MyStr : String;
begin
   Result:=Result+Result;
end;

function MyInt : Integer;
begin
   Result:=Result-Result;
end;

function MyFloat : Float;
begin
   Result:=Sqr(Result);
end;

PrintLn(MyStr);
PrintLn(MyInt);
PrintLn(MyFloat);
