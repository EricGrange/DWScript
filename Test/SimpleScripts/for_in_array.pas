const primaries : array [3..6] of Integer = [3, 5, 7, 9];

const colors : array [0..2] of String = ['red', 'green', 'blue'];

type
   TMyClass = class
      Field : String;
      constructor Create(f : String);
      begin
         Field:=f;
      end;
      procedure DoPrint;
      begin
         PrintLn(Field);
      end;
   end;

var objs : array of TMyClass;

objs.Add(new TMyClass('hello'));
objs.Add(TMyClass.Create('world'));

var i : Integer;
for i in primaries do
   PrintLn(i);
   
var c : String;
for c in colors do
   PrintLn(c);   
   
var o : TMyClass;
for o in objs do
   o.DoPrint;      
   
for i in primaries step 2 do
   PrintLn(i);
   