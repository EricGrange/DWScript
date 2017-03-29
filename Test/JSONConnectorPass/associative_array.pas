type 
   TPoint = record
      published 
         x, y : Integer;
   end;

var a : array [String] of TPoint;

var p : TPoint;
p.x := 1;
p.y := 2;
a['1.2'] := p;

p.x := 3;
p.y := 4;
a['3.4'] := p;

PrintLn(JSON.Stringify(a));

var b : array [Integer] of TObject;

b[10] := nil;
b[11] := new TObject;

type TTest = class
        Field := 'hello';
    end;
    
b[20] := new TTest;

type TCustom = class
        function Stringify : String; begin Result := '"world"' end;
    end;
    
b[21] := new TCustom;

PrintLn(JSON.Stringify(b));