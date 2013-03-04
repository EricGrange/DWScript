type
   TRoot = class
      published
         function Abst : String; virtual; abstract;
   end;

type
   TBase = class (TRoot)
      private
         Field : Integer;
      published
         procedure Hello(s : String); begin PrintLn('Hello '+s); end;
         function Virt(i : Integer) : Integer; virtual; begin Field+=i; Result:=Field; end;
         function Abst : String; override; begin Result:='Here'; end;
         class function World : String; static; begin Result:='world'; end;
         procedure Overlap; overload; virtual; begin PrintLn('Base');end;
   end;
   
type   
   TSub = class(TBase)
      published 
         function Virt(i : Integer) : Integer; override; begin Result:=inherited Virt(i)*2; end;
         function Abst : String; override; final; begin Result:='There'; end;
         procedure Overlap; reintroduce; virtual; begin PrintLn('Sub');end;
   end;
      
function CallMethod(instance : TObject; aClass : TClass; methodName : String; const params : array of const) : Variant;
begin
   var rtti := RTTIRawAttributes;
   while Assigned(aClass) do begin
      var typeID := TypeOf(aClass);
      var i : Integer;
      for i:=Low(rtti) to High(rtti) do begin
         var attrib := rtti[i];
         if (attrib.T = TypeID) and (attrib.A is RTTIMethodAttribute) then begin
            var meth := RTTIMethodAttribute(attrib.A);
			if meth.Name=methodName then 
				Result:=meth.Call(instance, params);
         end;
      end;
      aClass:=aClass.ClassParent;
   end;
end;

var b := new TBase;
var s := new TSub;

CallMethod(b, TBase, 'Hello', ['World']);
PrintLn(CallMethod(b, TBase, 'Virt', [123]));
PrintLn(CallMethod(b, TBase, 'Virt', [123]));
PrintLn(CallMethod(b, TBase, 'World', []));
PrintLn(CallMethod(b, TBase, 'Abst', []));
CallMethod(b, TBase, 'Overlap', []);

PrintLn(CallMethod(s, TSub, 'Virt', [123]));
PrintLn(CallMethod(s, TSub, 'Virt', [123]));
PrintLn(CallMethod(s, TSub, 'Abst', []));
CallMethod(s, TSub, 'Overlap', []);

