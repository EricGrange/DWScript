{

Special DWSII Syntax:

case-statement with non-ordinal datatypes

}

var s: string = 'hello';

case s of
  'hello': PrintLn('Hello!');
  'goodbye': PrintLn('Goodbye!');
end;

case s of
  'bug': PrintLn('bug');
else
  PrintLn('...');
end;


var b: Boolean;

case b of
  true: PrintLn('Default value of boolean is TRUE');
  false: PrintLn('Default value of boolean is FALSE');
end;

case b of
  true: PrintLn('Bug');
else
  PrintLn('false indeed');
end;
