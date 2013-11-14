type
   MyAttribute = class (TCustomAttribute);
   
type   
   [MyAttribute]
   TTest1 = class
   end;

type   
   [TCustomAttribute]
   TTest2 = class
   end;

type   
   [My]
   TTest3 = class
   end;

   
procedure PrintAttributesForType(typeID : TRTTITypeInfo);
begin
   var rtti := RTTIRawAttributes;
   var i : Integer;
   for i:=Low(rtti) to High(rtti) do
      if rtti[i].T = typeID then
         PrintLn(rtti[i].A.ClassName);
end;

PrintAttributesForType(TypeOf(TTest1));
PrintAttributesForType(TypeOf(TTest2));
PrintAttributesForType(TypeOf(TTest3));
