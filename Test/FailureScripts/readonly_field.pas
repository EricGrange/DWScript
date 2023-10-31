type
   TTest = class
      FMyField : Integer; readonly;
      property MyProp : Integer read FMyField
                                write FMyField;
      constructor Create; begin FMyField := 1; end;
   end;

var t := new TTest;
t.FMyField := 2;