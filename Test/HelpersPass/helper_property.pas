type
   THelper = helper for String
      function GetProp : Integer;
      begin
         PrintLn('GetProp');
         Result:=Length(Self);
      end;
      procedure SetProp(v : Integer);
      begin
         PrintLn('SetProp');
         PrintLn(Self);
         PrintLn(v);
      end;
      property Prop : Integer read GetProp write SetProp;
   end;
   
var s : String = 'hello';

PrintLn(s.Prop);
s.Prop:=123;
