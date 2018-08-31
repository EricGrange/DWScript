type 
    TTest = class
        i : Integer;
        f : Float;
        constructor Create(ai : Integer; af : Float); begin i := ai; f := af; end;
    end;
    
var a : array of TTest;

procedure PrintA;
begin
    for var t in a do
        PrintLn('   ' + t.i.ToString + ' x ' + t.f.ToString);
end;

a.Add(new TTest(1, 1.5));
a.Add(new TTest(2, 2.5));
a.Add(new TTest(1, 3.5));
a.Add(new TTest(2, 1.5));

PrintLn('initial');
PrintA;

PrintLn('sort by i then f');
a.Sort(lambda (a, b : TTest) => CompareNum(a.i, b.i) ?? CompareNum(a.f, b.f));
PrintA;

PrintLn('sort by f then i');
a.Sort(lambda (a, b : TTest) => a.f.Compare(b.f) ?? a.i.Compare(b.i));
PrintA;

PrintLn('sort by i then -f');
a.Sort(lambda (a, b : TTest) => a.i.Compare(b.i) ?? -a.f.Compare(b.f));
PrintA;

   
   
    