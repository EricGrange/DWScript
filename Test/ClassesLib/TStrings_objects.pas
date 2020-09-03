type
  TSValue = class
    Text: String;
    constructor Create(val: String);
  end;

constructor TSValue.Create(val: String);
begin
  Text:= val;
end;

var l := TStringList.Create;

l.AddObject('First', TSValue.Create('a'));
l.AddObject('Last', TSValue.Create('z'));
l.Insert(1, 'Extra');
l.InsertObject(1, 'Middle', nil);
PrintLn(l.IndexOf('Extra'));
l.Delete(2);
l.AddObject('Post', TSValue.Create('Mortem'));

var i: Integer;

for i:= 0 to l.Count - 1 do begin
  var s:= l[i];
  var o:= l.objects[i];
  if Assigned(o) then
    PrintLn(s + ': ' + (o as TSValue).Text)
  else
    PrintLn(s + ': nil');
end;
