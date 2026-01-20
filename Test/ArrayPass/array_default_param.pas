type Test = class
  procedure Test(a : Integer ; b : array of String = []);
  begin
   var s := b.Join(',');
   for var i := 1 to a do
     b.Add(a.ToString);
   PrintLn('[' + s + '] > [' + b.Join(',') + ']');
  end;
end;

var t := new Test;
t.Test(2);
t.Test(1);
t.Test(0);
t.Test(5, []);
t.Test(0);
