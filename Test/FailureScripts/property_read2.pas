type
   TMyClass = class
      FField : Integer;
      function GetField : Integer; begin Result:=FField; end;
      property Indirect : Integer read GetField;
   end;

var o:=TMyClass.Create;

var i := o.Indirect.bug;
