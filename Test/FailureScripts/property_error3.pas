var v : Integer;

type
   TMyClass = class
      Field : Integer;
      procedure DoIt;
      function GetIt : Integer;
      property Test1 : String write GetIT;
      property Test2 : String read GetIT;
      property Test3 : String write DoIT;
      property Test4 : String read DoIT;
      property Stuff : String read ;
   end;