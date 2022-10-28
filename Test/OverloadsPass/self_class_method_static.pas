type
   TTest123 = class
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest123.Create.Test;
TTest123.TestStatic;

type
   TTest132 = class
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest132.Create.Test;
TTest132.TestStatic;

type
   TTest213 = class
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest213.Create.Test;
TTest213.TestStatic;

type
   TTest231 = class
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest231.Create.Test;
TTest231.TestStatic;

type
   TTest312 = class
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest312.Create.Test;
TTest312.TestStatic;

type
   TTest321 = class
      procedure Over(a, b, c : Integer); overload; begin PrintLn(a.ToString+','+b.ToString+','+c.ToString) end;
      class procedure Over(a, b : Integer); overload; static; begin PrintLn(a.ToString+','+b.ToString) end;
      class procedure Over(a : Integer); overload; begin PrintLn(a.ToString) end;
      procedure Test; begin PrintLn(ClassName); Over(1); Over(2, 3); Over(4, 5, 6); end;
      class procedure TestStatic; begin Over(7); Over(8, 9); end;
   end;

TTest321.Create.Test;
TTest321.TestStatic;
