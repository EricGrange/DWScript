type
  TRec = record
    S: String; 
    B1: Boolean;
	I: Integer;
	F: Float;
	B2: Boolean;
	B3: Boolean;
	V: Variant;
	O1, O2: TObject;
  end;

var tmpA: array of TRec;

tmpA.SetLength(1);
tmpA[0].S := 'hello';
tmpA[0].B1 := True;
tmpA[0].I := 123;
tmpA[0].F := 1.5;
tmpA[0].B2 := False;
tmpA[0].B3 := True;
tmpA[0].V := Null;
tmpA[0].O1 := new TObject;
tmpA[0].O2 := nil;
PrintLn(if tmpA[0].S = 'hello' then 'Hello');
PrintLn(tmpA[0].B1 = True);
PrintLn(if tmpA[0].I = 123 then 1234);
PrintLn(if tmpA[0].F = 1.5 then 1.25);
PrintLn(tmpA[0].B2 = True);
PrintLn(tmpA[0].B3 = True);
PrintLn(if VarIsNull(tmpA[0].V) then 'null');
PrintLn(tmpA[0].O1.ClassName);
PrintLn(tmpA[0].O2 = nil);
