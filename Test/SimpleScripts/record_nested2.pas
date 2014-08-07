type
	TSub = record
		D : String;
	end;
	
type
	TParent = record
		A : String;
		Sub : record
			C : TSub;
		end;
	end;

var r1, r2 : TParent;

r1.A:='hello';
r1.Sub.C.D:='world';

r2:=r1;

PrintLn(r2.A);
PrintLn(r2.Sub.C.D);