type
   TMyFunc = procedure (v : Variant);

var f : TMyFunc := Print;

f('hello ');
f:=PrintLn;
f('world');

function MyPrint(b : Boolean) : TMyFunc;
begin
   if b then 
      Result:=PrintLn
   else Result:=Print;
end;

MyPrint(True)('Line Feed');
MyPrint(False)('No');
MyPrint(False)('Line Feed');

var a : array [0..1] of TMyFunc;

a[0]:=Print;
a[1]:=PrintLn;

a[1]('!');
a[0]('No');
a[0]('Line Feed');
a[1]('Line Feed');

