type TTest = helper for Integer
   procedure Hello(a : Integer); overload; begin Hello(a, a); end;
   procedure Hello(a, b : Integer); overload; begin PrintLn(a+b); end;
end;