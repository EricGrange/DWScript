type
   TMyClass = class
      Field : String;
      procedure Hello;
   end;

type
   TSubClass1 = class(TMyClass)
      constructor World; default;
   end;

type
   TSubClass2 = class(TMyClass)
      constructor World;
   end;

procedure TMyClass.Hello;
begin
   Print('Hello ');
   PrintLn(Field);
end;

constructor TSubClass1.World;
begin
   Field:="World1";
end;

constructor TSubClass2.World;
begin
   Field:="World2";
end;

var o : TMyClass;

o:=new TMyClass;
o.Hello;
o:=new TSubClass1();
o.Hello;
new TSubClass1.Hello;
new TSubClass2().Hello;
