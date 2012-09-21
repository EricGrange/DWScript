type
   TBase = class
      published
         Field : Integer = 1;
   end;
   
type   
   TSub = class(TBase)
      public
         Hidden : String;
      published 
         SubField : String = 'hello';   
   end;
      
procedure PrintPropertiesForType(obj : TObject);
begin
   var rtti := RTTIRawAttributes;
   var aClass := obj.ClassType;
   PrintLn(aClass.ClassName+':');
   while Assigned(aClass) do begin
      var typeID := TypeOf(aClass);
      var i : Integer;
      for i:=Low(rtti) to High(rtti) do begin
         var attrib := rtti[i];
         if (attrib.T = TypeID) and (attrib.A is RTTIPropertyAttribute) then begin
            var prop := RTTIPropertyAttribute(attrib.A);
            Print('- '+prop.Name);
            Print(' : ');
            Print(prop.Typ.Name);
            Print(' = ');
            var v := prop.Getter(obj);
            PrintLn(v);
            prop.Setter(obj, v+v);
         end;
      end;
      aClass:=aClass.ClassParent;
   end;
end;

var b := new TBase;
var s := new TSub;

PrintPropertiesForType(b);
PrintPropertiesForType(s);

PrintLn(b.Field);
PrintLn(s.SuBField);