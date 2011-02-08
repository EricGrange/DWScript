type 
   TClassA = class
      Field : String;
      constructor Create; virtual;
   end;

type
   TClassB = class(TClassA)
      constructor Create; override;
   end;

constructor TClassA.Create;
begin
   Field := 'A';
end;

constructor TClassB.Create;
begin
   Field := 'B';
end; 

var o, o2 : TClassA;

o:=TClassA.Create;
PrintLn(o.Field);
o2:=o.Create;
PrintLn(o2.Field);

o:=TClassB.Create;
PrintLn(o.Field);
o2:=o.Create;
PrintLn(o2.Field);
