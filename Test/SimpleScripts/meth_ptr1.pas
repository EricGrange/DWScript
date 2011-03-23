type
   TMyProc = procedure;

type
   TClassA = class
      Field : String;
      procedure StaticProc;
      procedure VirtualProc; virtual;
   end;

type
   TClassB = class(TClassA)
      class procedure ClassProc;
      procedure VirtualProc; override;
   end;

procedure TClassA.StaticProc;
begin
   PrintLn(ClassName+' StaticProc '+Field);
end;

procedure TClassA.VirtualProc;
begin
   PrintLn(ClassName+' VirtualProc '+Field);
end;

class procedure TClassB.ClassProc;
begin
   PrintLn(ClassName+' ClassProc');
end;

procedure TClassB.VirtualProc;
begin
   PrintLn(ClassName+' VirtualProc overriden '+Field);
end;

var a := TClassA.Create;
var b := TClassB.Create;

var p : TMyProc;

a.Field:='Alpha';
b.Field:='Beta';

p:=a.StaticProc;
p;
p:=a.VirtualProc;
p;
p:=b.StaticProc;
p;
b.ClassProc;
p;
p:=b.VirtualProc;
p;
p:=TClassB.ClassProc;
p;

a:=b;
p:=a.StaticProc;
p;
p:=a.VirtualProc;
p;