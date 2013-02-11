type
   TStrArray = array of string;

type
   TTest = class
      constructor CreateIt(o: TObject; s: string); overload;
      constructor CreateIt(o: TObject; s: string; const a: array of String); overload;
   end;

constructor TTest.CreateIt(o: TObject; s: string);
begin
   PrintLn('First:');
   PrintLn(o.ClassName);
   PrintLn(s);
end;

constructor TTest.CreateIt(o: TObject; s: string; const a: array of String);
begin
   PrintLn('Second:');
   PrintLn(o.ClassName);
   PrintLn(s);
   PrintLn(StrJoin(a, ','));
end;

var o := TTest.CreateIt(TObject.Create, 'hello');

TTest.CreateIt(o, 'world', ['a', 'b']);