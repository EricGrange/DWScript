type TTest = helper for Integer
   class procedure Hello(a, b : Integer); overload; begin PrintLn(a+b); end;
   class procedure Hello(a : Integer); overload; begin Hello(a, a); end;
end;

var i := 123;
i.Hello(10);
i.Hello(11, 100);

Integer.Hello(20);
Integer.Hello(13, 100);
