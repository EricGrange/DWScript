type
   TTest<A> = class
      Field : A;
      function Get : A;
      begin
         Result := Field + Field;
      end;
      procedure Set(v : A);
      begin
         Field := v + v;
      end;
      property Prop : A read Get write Set;
   end;

var ti := new TTest<Float>;
ti.Prop := 1.5;
PrintLn(ti.Field);
PrintLn(ti.Prop);

var ts := new TTest<String>;
ts.Prop := 'a';
PrintLn(ts.Field);
PrintLn(ts.Prop);