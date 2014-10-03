type
   TSub = class
      procedure setVal(aKey, aValue: String);
      begin
         PrintLn(aKey+','+aValue);
      end;

      property Values[aKey: String]: String write setVal; default;
   end;

type
   TTest = class
     FSub : TSub;
     property Sub : TSub read FSub;
  end;

var data := new TTest;

data.Sub.Values['ABC'] := '123';
data.Sub['DEF'] := '456';
