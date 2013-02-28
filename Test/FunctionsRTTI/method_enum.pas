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
      
procedure PrintMethods(aClass : TClass);
begin
   var rtti := RTTIRawAttributes;
   var spottedVMTIndexes : array of Integer;
   PrintLn(aClass.ClassName+':');
   while Assigned(aClass) do begin
      var typeID := TypeOf(aClass);
      var i : Integer;
      for i:=Low(rtti) to High(rtti) do begin
         var attrib := rtti[i];
         if (attrib.T = TypeID) and (attrib.A is RTTIMethodAttribute) then begin
            var meth := RTTIMethodAttribute(attrib.A);
            if (meth.VMTIndex>=0) and (spottedVMTIndexes.IndexOf(meth.VMTIndex)>=0) then
               continue;
            spottedVMTIndexes.Add(meth.VMTIndex);
            if (meth.Info and meth.infoClass)<>0 then 
               Print('class ');
            if meth.Typ.Name<>'' then begin
               Print('function '+aClass.ClassName+'.'+meth.Name);
               Print(' : '+meth.typ.Name);
            end else begin
               Print('procedure '+aClass.ClassName+'.'+meth.Name);
            end;
            if meth.VMTIndex>=0 then 
               Print('; virtual '+IntToStr(meth.VMTIndex));
            var info := meth.Info;
            if (info and meth.infoAbstract)<>0 then 
               Print('; abstract');
            if (info and meth.infoOverride)<>0 then 
               Print('; override');
            if (info and meth.infoFinal)<>0 then 
               Print('; final');
            if (info and meth.infoOverload)<>0 then 
               Print('; overload');
            if (info and meth.infoOverlap)<>0 then 
               Print('; overlap');
            if (info and meth.infoStatic)<>0 then 
               Print('; static');
            PrintLn(';')
         end;
      end;
      aClass:=aClass.ClassParent;
   end;
end;

var b := new TBase;
var s := new TSub;

PrintMethods(TRoot);
PrintMethods(b.ClassType);
PrintMethods(s);