var v : Integer;

type
   TMyClass = class
      Field : Integer;
      function GetTest(i : Integer) : Integer;
      property Test1 : Integer index v read GetTest write Field;
      property Test2[i : Integer] : Integer index 0 read Field write GetTest;
      property Stuff : Integer;
      property Dummy bug;
   end;