type
   TMyArray = array [1..2] of String;

type
   THelper = helper for TMyArray
      function ToString : String;
      begin
         Result:=Self[1]+';'+Self[2];
      end;
      class function NewOne(a, b : String) : TMyArray;
      begin
         Result[1]:=a;
         Result[2]:=b;
      end;
      const ByeBye : TMyArray = ['Bye', 'bye'];
   end;
   
var v := TMyArray.NewOne('hello', 'world');

PrintLn(v.ToString);

PrintLn(TMyArray.ByeBye.ToString);