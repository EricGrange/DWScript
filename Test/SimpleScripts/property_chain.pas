type
   TClass1 = class
      Field : String;
      procedure Anything;
      property Prop : String read Field write Field;
   end;

procedure TClass1.Anything;
begin
   PrintLn('here');
end;

type
   TClass2 = class
      fClass1Item : TClass1;
      function NeedClass1 : TClass1;
      property Class1Item : TClass1 read fClass1Item;
   end;

function TClass2.NeedClass1 : TClass1;
begin
   if not Assigned(fClass1Item) then begin
      fClass1Item:=TClass1.Create;
      PrintLn('Class1 needed');
   end;
   Result:=fClass1Item;
end;

var class2Item : TClass2;

class2Item:=TClass2.Create;
class2Item.NeedClass1.Anything;
class2Item.Class1Item.Anything;

class2Item.Class1Item.Field:='hello';
PrintLn(class2Item.Class1Item.Field);
