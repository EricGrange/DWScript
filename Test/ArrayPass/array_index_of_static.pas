var a := [ 1, 2, 3 ];
   
var b := a; 

b[0] := 3;
b[1] := 1;
b[2] := 2;

for var i := 0 to 4 do
    Print(a.IndexOf(i));

PrintLn('');

for var i := 4 downto 0 do
    Print(b.IndexOf(i));

PrintLn('');
    
var r : array [ 1..2 ] of record x, y : String end = [
    ( x: "hello"; y : "world" ),
    ( x: "Hello"; y : "World" )
];

var rr := r[2];

PrintLn(r.IndexOf(rr));
rr.x := 'hello';
PrintLn(r.IndexOf(rr));
rr.y := 'world';
PrintLn(r.IndexOf(rr));
    
