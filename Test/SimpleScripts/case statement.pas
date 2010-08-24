{

Special DWSII Syntax:

case-statement with non-ordinal datatypes

}

var s: string = 'hello';

case s of
  'hello': PrintLn('Hello!');
  'goodbye': PrintLn('Goodbye!');
end;

var b: Boolean;

case b of
  true: PrintLn('Default value of boolean is TRUE');
  false: PrintLn('Default value of boolean is FALSE');
end;
