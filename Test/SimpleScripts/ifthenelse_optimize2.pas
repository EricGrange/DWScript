//
// New Script
//
const B = False;

type
   TTest = class
      function Get(index : Integer) : TTest; begin PrintLn(index); Result := nil; end;
      property P[index : Integer] : TTest read Get;
      procedure Set(v : String); begin PrintLn(v) end;
      property S : String write Set;
   end;

var t : TTest;

t.P[1].S:='a';
if B then
	t.P[2].S:='b'
else t.P[3].S:='c';

if not B then
	t.P[4].S:='d'
else t.P[5].S:='e';

if B then begin
	if t<>nil then
		t.P[6].S:='f'
	else t.P[7].S:='g';
end else begin
	if t<>nil then
		t.P[8].S:='h'
	else t.P[9].S:='i';
end;
