var i : Integer;
i:=123;

function Get123 : array [1..3] of Integer;
begin
   Result:=[1, 2, 3];
end;

var r := 
   record
      fi := i;
      fi2 := i*2;
      fis := '<'+IntToStr(i)+'>';
      f123 := Get123;
      property pi : Integer read fi;
      function pis : string;
      begin
         Result:='['+IntToStr(pi)+']';
      end;
   end;
   
PrintLn(r.fi);
PrintLn(r.fi2);
PrintLn(r.fis);
PrintLn(r.pis);
PrintLn(r.f123[1]);
PrintLn(r.f123[2]);
PrintLn(r.f123[3]);

var r2 := r;
r.fi:=456;

PrintLn(r.pi);
PrintLn(r2.pi);
PrintLn(r2.fis);

PrintLn(r.pis);
PrintLn(r2.pis);

PrintLn(r2.f123[1]);
PrintLn(r2.f123[2]);
PrintLn(r2.f123[3]);
