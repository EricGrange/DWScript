type
   TRec = record
      a : Boolean;
      b : TObject;
   end;

var r : TRec;

PrintLn(r.a);

r.a:=True;
r.b:=new TObject;

PrintLn(r.a);
PrintLn(r.b.ClassName);

r.a:=not r.a;

PrintLn(r.a);

