type
   TTest123 = class
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest123.Test(1);
TTest123.Test(2, 3);
TTest123.Test(4, 5, 6);

type
   TTest132 = class
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
   end;

TTest132.Test(1);
TTest132.Test(2, 3);
TTest132.Test(4, 5, 6);

type
   TTest213 = class
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest213.Test(1);
TTest213.Test(2, 3);
TTest213.Test(4, 5, 6);

type
   TTest231 = class
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
   end;

TTest231.Test(1);
TTest231.Test(2, 3);
TTest231.Test(4, 5, 6);

type
   TTest312 = class
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
   end;

TTest312.Test(1);
TTest312.Test(2, 3);
TTest312.Test(4, 5, 6);

type
   TTest321 = class
      class procedure Test(a, b, c : Integer); overload; virtual; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + c.ToString + ', ' + b.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn(ClassName + ': ' + a.ToString + ', ' + b.ToString); end;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
   end;

TTest321.Test(1);
TTest321.Test(2, 3);
TTest321.Test(4, 5, 6);
