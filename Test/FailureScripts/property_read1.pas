type
   TMyClass = class
      FField : Integer;
      function GetField : Integer; begin Result:=FField; end;
      property Direct : Integer read FField;
      property Indirect : Integer read GetField;
   end;

var i : Integer;

i := TMyClass.Direct;

i := TMyClass.Indirect;
