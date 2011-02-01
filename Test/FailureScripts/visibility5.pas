type
   TTest = class
      private
         Field : Integer;
   end;
type
   TSubTest = class (TTest)
      public
         property Prop : Integer read Field;
   end;


var o := TSubTest.Create;
o.Prop:=1;