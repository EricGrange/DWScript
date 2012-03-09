type
   TProc = procedure;

type
   TMyClass = class
      FField : TProc;
      procedure SetField(v : TProc); begin FField:=v; end;
      property Indirect : TProc write SetField;
      procedure Meth;
   end;

procedure Proc1; forward;   
   
var o:=TMyClass.Create;

o.Indirect:=Proc1;
o.Indirect:=o.Meth;
o.Indirect:=TMyClass.Meth;
o.Indirect:=bug;
