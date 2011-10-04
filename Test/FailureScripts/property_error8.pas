type
   TMyClass = class
      class procedure SetVal(v : Integer);
      property Val : Integer write SetVal;
      
      procedure SetProp(v : Integer);
      property Prop : Integer write SetProp;
   end;

TMyClass.Val:='123';   
TMyClass.Prop:=123;