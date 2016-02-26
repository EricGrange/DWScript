type
   TTest = class
      Field : TTest;
      property Prop : TObject read Field;
   end;
type
   THelperObject = strict helper for TObject
      procedure Test; begin PrintLn('Object '+Self.ClassName) end;
   end;
type
   THelperTest = strict helper for TTest
      procedure Test; begin PrintLn('My '+Self.ClassName) end;
   end;

var o := new TTest;
o.Field := o;

o.Field.Test;
o.Prop.Test;