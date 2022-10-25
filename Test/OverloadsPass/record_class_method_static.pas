type
   TTest123 = record
      Field: Integer = 123;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest123.Test(1);
TTest123.Test(2, 3);
var r123 : TTest123;
r123.Test(4, 5, 6);

type
   TTest132 = record
      Field: Integer = 132;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest132.Test(1);
TTest132.Test(2, 3);
var r132 : TTest132;
r132.Test(4, 5, 6);

type
   TTest213 = record
      Field: Integer = 213;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest213.Test(1);
TTest213.Test(2, 3);
var r213 : TTest213;
r213.Test(4, 5, 6);

type
   TTest231 = record
      Field: Integer = 231;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest231.Test(1);
TTest231.Test(2, 3);
var r231 : TTest231;
r231.Test(4, 5, 6);

type
   TTest312 = record
      Field: Integer = 312;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest312.Test(1);
TTest312.Test(2, 3);
var r312 : TTest312;
r312.Test(4, 5, 6);

type
   TTest321 = record
      Field: Integer = 321;
      class procedure Test(a : Integer); overload; static; begin PrintLn('one: ' + a.ToString); end;
      class procedure Test(a, b : Integer); overload; begin PrintLn('two: ' + a.ToString + ', ' + b.ToString); end;
      procedure Test(a, b, c : Integer); overload; begin PrintLn(Field.ToString + ': ' + a.ToString + ', ' + b.ToString + ', ' + c.ToString); end;
   end;

TTest321.Test(1);
TTest321.Test(2, 3);
var r321 : TTest321;
r321.Test(4, 5, 6);
