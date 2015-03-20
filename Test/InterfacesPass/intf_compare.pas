type IIntf1 = interface
      function Foo: Integer;
   end;

type IIntf2 = interface
      function Bar: Integer;
   end;

type TClass1 = class(TObject, IIntf1)
      function Foo: Integer; begin Result := 1; end;
   end;

type TClass2 = class(TObject, IIntf2)
      function Bar: Integer; begin Result := 2; end;
   end;

var Obj1 := TClass1.Create;
var Obj2 := TClass2.Create;
var Intf1: IIntf1 := Obj1;
var Intf2: IIntf2 := Obj2;
var Intf3 := Intf1;

if Intf1 = Intf2 then PrintLn('Fail');
if Intf1 = Intf3 then PrintLn('Ok');