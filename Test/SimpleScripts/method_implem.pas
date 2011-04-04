type 
   TMyClass = class
      FField : Integer;
	  procedure SetField(f : Integer); begin FField:=MaxInt(0, f); end;
	  property Field : Integer read FField write SetField;
	  procedure PrintMe; virtual; begin PrintLn(Field); end;
   end;
   
type   
   TMySubClass = class (TMyClass)
      procedure PrintMe; override; begin Print('Sub-'); inherited; end;
      const cHello = 'Hello';
	  class function Hello : String; begin Result:=cHello; end;
   end;
   
var o := TMyClass.Create;
o.Field:=3;
o.PrintMe;
o.Field:=-o.Field; 
o.PrintMe;

PrintLn(TMySubClass.Hello);

o:=TMySubClass.Create;
o.Field:=3;
o.PrintMe;
PrintLn((o as TMySubClass).Hello);