var a : array of array of String;

procedure PrintA;
begin
   PrintLn(a.Map(lambda (e) => e.Join(',')).Join(';'));
end;

a.Add(('a-b').Split('-'), ('c-d').Split('-'));

PrintA;

a.Move(0, 1);
PrintA;

a.Move(1, 0);
PrintA;

a.Insert(1, ('foo').Split(','));

a.Move(0, 2);
PrintA;

a.Move(2, 0);
PrintA;

a.Reverse;
PrintA;

a.Add(a.Copy);
PrintA;