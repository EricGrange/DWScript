type Toto=Class(TObject)
public
   s : String;
   i : Integer;
   f : Float;
   b : Boolean;
   o : TObject;
end;

var obj: Toto;

procedure tataS(var chaine: String);
begin
   chaine:='tata';
end;

procedure tataI(var int: Integer);
begin
   int:=456;
end;

procedure tataF(var fl: Float);
begin
   fl:=4.5;
end;

procedure tataB(var b: Boolean);
begin
   b:=True;
end;

procedure tataO(var o: TObject);
begin
   o:=nil;
end;


obj:=Toto.Create;
obj.s:='toto';
obj.i:=123;
obj.f:=1.5;
obj.o:=obj;

PrintLn(obj.s);

tataS(obj.s);
tataI(obj.i);
tataF(obj.f);
tataB(obj.b);
tataO(obj.o);

PrintLn(obj.s);
PrintLn(obj.i);
PrintLn(obj.f);
PrintLn(obj.b);
if obj.o<>nil then
   PrintLn('bug');