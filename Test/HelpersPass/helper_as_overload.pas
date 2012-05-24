type
   Vec2 = array [0..1] of Float;
   
type
   TVec2Helper = helper for Vec2
      function Add(const v : Vec2) : Vec2; overload;
      begin
         Result:=[Self[0]+v[0], Self[1]+v[1]];
      end;
      function Add(const f : Float) : Vec2; overload;
      begin
         Result:=[Self[0]+f, Self[1]+f];
      end;
   end;
   
operator + (Vec2, Vec2) : Vec2 uses TVec2Helper.Add;
operator + (Vec2, Float) : Vec2 uses TVec2Helper.Add;

var v1 : Vec2 := [1, 2];
var v2 : Vec2 := [10, 20];

var v := v1 + v2;

PrintLn(v[0]);
PrintLn(v[1]);

v := v1 + 5.0;

PrintLn(v[0]);
PrintLn(v[1]);
