type 
   TRec = record
      w, x, y : Integer;
      z : Float;
      name : String;
   end;

var c : TRec;
var i : Integer;

c:=const TRec(x: 1; y: i; z:3.1; name : 'hello');
c:=const TObject(x: 1; y: 2; z:3.1; name : 'hello');
