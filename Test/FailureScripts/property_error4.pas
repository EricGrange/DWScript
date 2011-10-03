var v : Integer;

type
   TMyClass = class
      Field : Integer;
      procedure DoIt;
      function GetIt(i : Integer) : Integer;
      property Test[i : String] : Integer read GetIT;
      property Prop : String write Field;
      property Stuff : String write ;
   end;