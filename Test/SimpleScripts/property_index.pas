type TTest = class
      function GetProp(i : Integer) : String;
      procedure SetProp(i : Integer; v : String);

      property Items[i : Integer] : String read GetProp write SetProp; default;
      property Item1 : String index 1 read GetProp write SetProp;
      property Item2 : String index 2 read GetProp write SetProp;
   end;

function TTest.GetProp(i : Integer) : String;
begin
   Result:='Get '+IntToStr(i);
end;

procedure TTest.SetProp(i : Integer; v : String);
begin
   PrintLn('Set '+IntToStr(i)+' with '+v);
end;

var o := TTest.Create;

PrintLn(o[1]);
PrintLn(o[2]);
o[1]:='hello';
o[2]:='world';

PrintLn(o.Item1);
PrintLn(o.Item2);

o.Item1:='bye bye';
o.Item2:='test script';

