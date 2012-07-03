
type TMyArray=array[1..1000] of integer;
var MyArray:TMyArray;

procedure swapint(var a,b:integer);
begin
  var z:integer;
  z:=a;
  a:=b;
  b:=z;
end;

// Fastsearch - recursive version

function FastSearchRec(lower,upper,tosearch:integer):integer;
begin
 var m1,m2:integer;
 if lower <= upper then
 begin 
  m1 := trunc((lower+upper)/2);
  m2 := lower+trunc((upper-lower)*(tosearch-MyArray[lower])/(MyArray[upper]- MyArray[lower]));
  if m1 > m2 then swapint(m1,m2);
  if tosearch = MyArray[m1] then result := m1
  else if tosearch = MyArray[m2] then result := m2
  else if tosearch < MyArray[m1] then result := fastsearchrec(lower,m1-1,tosearch)
  else if tosearch < MyArray[m2] then result := fastsearchrec(m1+1,m2-1,tosearch)
  else result := fastsearchrec(m2+1,upper,tosearch);
 end else result := -1;
end;

// Fastsearch - iterative version

function FastSearchIt(lower,upper,tosearch:integer):integer;
begin
 var m1,m2, pos:integer;
 pos := -1;
 repeat
  m1 := trunc((lower+upper)/2);
  m2 := lower+trunc((upper-lower)*(tosearch-MyArray[lower])/(MyArray[upper]- MyArray[lower]));
  if m1 > m2 then swapint(m1,m2);
  if tosearch = MyArray[m1] then pos := m1
  else if tosearch = MyArray[m2] then pos := m2
  else if tosearch < MyArray[m1] then upper:=m1-1
  else if tosearch > MyArray[m2] then lower:=m2+1
  else begin lower := m1+1; upper := m2-1; end;
 until (pos <> -1) or (MyArray[lower]>=MyArray[upper]);
 if tosearch=MyArray[lower] then pos := lower;
 result := pos;
end;

var i : integer;
for i:= 1 to 1000 do MyArray[i] := i;
println(inttostr(FastSearchRec(1,1000,500)));
println(inttostr(FastSearchIt(1,1000,500)));
