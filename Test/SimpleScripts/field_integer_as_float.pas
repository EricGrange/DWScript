type TRec = record
  I: Integer;
end;

type TObj = class
	I : Integer;
end;

var P: TRec;
P.I:=1;
var O:= TObj.Create;
O.I:=2;

var F: Float;

F := P.I;
PrintLn(F);

F := O.I;
PrintLn(F);