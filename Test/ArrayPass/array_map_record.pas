type 
    TRec = record
        a, b : Integer;
    end;
    
var a : array of Integer;

for var i := 1 to 5 do
    a.Add(i*13);
   
function Decomp(i : Integer) : TRec;
begin
    Result.a := i div 10;
    Result.b := i mod 10; 
end;   
   
var b := a.Map(Decomp);

if a.Length <> b.Length then Print('bug 1');

function Recomp(r : TRec) : Integer;
begin
    Result := r.a * 100 + r.b * 10;
end;

var c := b.Map(Recomp);

if a.Length <> c.Length then Print('bug 2');

for var i := a.Low to a.High do
    PrintLn(
          a[i].ToString 
        + ' > ' + b[i].a.ToString + ',' + b[i].b.ToString
        + ' > ' + c[i].ToString
        );
   