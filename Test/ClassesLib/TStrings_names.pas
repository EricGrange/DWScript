var l:= TStringList.Create;

l.Add('Name=Value');
l.Values['Test']:= 'one';
l.Values['Final']:= 'Test*' + l.Values['Name'];
l.ValueFromIndex[1]:= 'Two';
l.Add('Plain');
l.ValueFromIndex[3]:= 'Simple';

var i: integer;

for i:= 0 to l.count - 1 do
  PrintLn(l.Names[i] + ', ' + l.ValueFromIndex[i]);
