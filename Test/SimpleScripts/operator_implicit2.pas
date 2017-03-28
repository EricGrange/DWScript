type TRec1 = record
    a, b : Integer;
end;

type TRec2 = record
    x, y : String;
end;

function Rec1ToRec2(r1 : TRec1) : TRec2;
begin
    Result.x := IntToStr(r1.a+r1.b);
    Result.y := IntToStr(r1.a*r1.b);
end;

operator implicit (TRec1) : TRec2 uses Rec1ToRec2;

procedure PrintRec2(r2 : TRec2);
begin
    PrintLn(r2.x);
    PrintLn(r2.y);
end;

var r1 : TRec1 = (a: 3; b: 7);
var r2 : TRec2;

r2 := r1;
PrintRec2(r2);

r1.a := 11;

PrintRec2(r1);