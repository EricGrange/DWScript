type TMyInteger = Integer;
type
   TTest = class
      Field : Integer;
      property Prop : TMyInteger read Field;
   end;
type
   THelperInteger = strict helper for Integer
      procedure Test; begin PrintLn('Integer '+Self.ToString) end;
   end;
type
   THelperMyInteger = strict helper for TMyInteger
      procedure Test; begin PrintLn('My '+Self.ToString) end;
   end;

var o := new TTest;
o.Field := 123;

o.Field.Test;
o.Prop.Test;