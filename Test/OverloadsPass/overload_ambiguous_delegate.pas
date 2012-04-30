type TFunc = function : Integer;

type
   TA = class
      procedure A(i : Integer); overload; begin PrintLn('A int')end;
      procedure A(f : TFunc); overload; begin PrintLn('A func')end;
   end;
   
type
   TB = class
      procedure B(f : TFunc); overload; begin PrintLn('B func')end;
      procedure B(i : Integer); overload; begin PrintLn('B int')end;
   end;
   
function Fn : Integer;
begin
   Result:=1;
end;

var a := TA.Create;
var b := TB.Create;

// ambiguous given the overloads
a.A(fn);
b.B(fn);

// not ambiguous, should pass
a.A(fn());
b.B(fn());
a.A(@fn);
b.B(@fn);

