type
   TParent = record
      A : string;
      Sub : record
			B : String;
		end;
   end;

var r1, r2 : TParent;

r1.A:='hello';
r1.Sub.B:='world';

r2:=r1;

PrintLn(r2.A);
PrintLn(r2.Sub.B);