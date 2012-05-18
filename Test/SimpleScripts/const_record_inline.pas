type 
   TRec = record
      w, x, y : Integer;
      z : Float;
      name : String;
   end;

procedure PrintRec(const c : TRec);
begin
   PrintLn(c.w);
   PrintLn(c.x);
   PrintLn(c.y);
   PrintLn(c.z);
   PrintLn(c.name);
end;   
   
const c : TRec = (x: 1; y: 2; z:3.1; name : 'hello');

PrintRec(c);
PrintRec(const TRec (x: 2; y: 3; z:4.5; name : 'world'));