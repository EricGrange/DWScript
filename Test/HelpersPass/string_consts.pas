type
   TDummy = helper for String
      const Hello = 'Hello';
      class var World = 'World';
   end;

PrintLn(String.Hello);
Print(String.World);
String.World:='!';
PrintLn(String.World);

var s : String;

Print(s.Hello);
Print(s.World);
s.World:='...';
PrintLn(s.World);

PrintLn(IntToStr(123).Hello);

Print(TDummy.Hello);
PrintLn(TDummy.World);
