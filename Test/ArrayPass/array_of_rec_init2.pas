type
   TRec = record
      A : Integer;
	  B : String;
	  c : Boolean;
   end;

var a : array [0..1] of TRec  = [
	(A: 1; b: "One"; c: True),
	(A: 2; B: 'two'; c: False)
	];

for var i in a do begin
	PrintLn(i.A);
	PrintLn(i.B);
	PrintLn(i.C);
end;
