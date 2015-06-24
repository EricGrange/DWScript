type MyString = String;

type TMyHelper = strict helper for MyString
      const Hello = 'Hello';
   end;

var s : String;

PrintLn(s.Hello);

type TMyBug = strict for String end;

