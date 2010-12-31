type
   TTest = class
      Field : Integer;
      function GetDef(i : Integer) : Integer;	
      procedure SetDef(i, v : Integer);
      property Def[i : Integer] : Integer read GetDef write SetDef; default;
      property Prop : Integer read Field write Field;
   end;

var t : TTest;

t[1];
t.Prop;
t.Field;