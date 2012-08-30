type
   TBase = class
      private 
         Field : Integer = 1;
         function GetField : Integer;
         begin
            Result:=Field*10;
         end;
         procedure SetField(v : Integer);
         begin
            Field:=v div 10;
         end;
         
      published
         property Direct : Integer read Field write Field;
         property Indirect : Integer read GetField write SetField;
   end;
   
type   
   TSub = class(TBase)
      protected
         SubField : String = 'hello';   
      public
         property Hidden : String read SubField write SubField;
      published 
         property Stuff : String read SubField write SubField;
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
            Print(', '+IntToStr(prop.Capabilities));
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

PrintLn(b.Direct);
PrintLn(s.Stuff);