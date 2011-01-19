type
   TMyClass = class
      function Test1(s : String) : Integer;
      function Test2(i : Integer) : Boolean;
      function Test3 : Boolean;

      class operator IN String uses Test1;
      class operator IN Float uses Test2;
      class operator IN TObject uses Test3;
   end;