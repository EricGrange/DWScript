//
// New Script
//
type
   TObj1 = class
      Field : TObject;
      property Prop : TObject read Field;
		function Get : TObject;
   end;

type
   TObj2 = class
      Field : TObj1;
      function GetItem : TObj1;
      function GetObjField : TObject;
      function GetObjProp : TObject;
		function GetObjGet : TObject;
   end;
	
function TObj1.Get : TObject;
begin
   Result:=Field;
end;

function TObj2.GetItem : TObj1;
begin
   Result:=Field;
end;

function TObj2.GetObjField : TObject;
begin
   Result:=GetItem.Field;
end;

function TObj2.GetObjProp : TObject;
begin
   Result:=GetItem.Prop;
end;

function TObj2.GetObjGet : TObject;
begin
   Result:=GetItem.Get;
end;

var o1 := new TObj1;
var o2 := new TObj2;
o2.Field:=o1;
o1.Field:=new TObject;

PrintLn(o2.GetItem.ClassName);
PrintLn(o2.GetObjField.ClassName);
PrintLn(o2.GetObjProp.ClassName);
PrintLn(o2.GetObjGet.ClassName);


