type
   TMyClass = class
   end;

var objA : TMyClass = TMyClass.Create;
var objB : TMyClass = TMyClass.Create;
var objC : TMyClass;

if objA=objB then PrintLn('A = B');
if objA<>objB then PrintLn('A <> B');

if objA=objC then PrintLn('A = C');
if objA<>objC then PrintLn('A <> C');

objC := objA;

if objA=objC then PrintLn('A = C');
if objA<>objC then PrintLn('A <> C');

   