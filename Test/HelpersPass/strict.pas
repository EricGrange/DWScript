type MyString = String;

type TMyHelper = strict helper for MyString
      const Hello = 'Hello';
   end;

var s : MyString;

PrintLn(s.Hello);

