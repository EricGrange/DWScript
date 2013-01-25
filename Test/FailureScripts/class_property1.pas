type
   TTest = class
      Field : Integer;
   
      procedure Hello(s : String); begin end;
      function World : String; begin Result:=''; end;
      
      class property Prop : String read World write Hello;
      class property Prop2 : Integer read Field write Field;
   end;