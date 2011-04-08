type
   TBase = class
      Field1 : Integer;
      property Prop : Integer read Field1;
   end;
type
   TChild = class (TBase)
      Field2 : Integer;
      property Prop : Integer read Field2;
      function GetBase : Integer; begin result:=inherited Prop; end;
      function GetChild : Integer; begin result:=Prop; end;
   end;

var o := TChild.Create;

o.Field1:=1;
o.Field2:=2;

PrintLn(o.GetBase);
PrintLn(o.GetChild);
