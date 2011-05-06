type 
   TRec = record
      w, x, y : Integer;
      z : Float;
      name : String;
   end;

const c : TRec = (x: 1; y: 2; z:3.1; name : 'hello');

PrintLn(c.w);
PrintLn(c.x);
PrintLn(c.y);
PrintLn(c.z);
PrintLn(c.name);