type C1 = class
   function F1(a : string) : array of string ; overload ;
   function F1(a : Integer) : array of string ; overload ;
 end;

function C1.F1(a: String): array of String ;
begin
   Result.Add("a", a);
end;

function C1.F1(a: Integer): array of String ;
begin
   Result.Add("b", a.ToString);
end;

var o := new C1;
PrintLn(o.F1('hello').Join(','));
PrintLn(o.F1(123).Join(','));