type
   TBase = class 
      class var Field : Integer = 1;
      class property Field1 : integer read Field write (Field);
      class property MultBy2 : Integer read (2*Field) write (Field := Value div 2);
   end;

type
   TBaseRec = record
      Dummy : Integer;
      class var Field : Integer = 10;
      class property Field1 : integer read Field write (Field);
      class property MultBy2 : Integer read (2*Field) write (Field := Value div 2);
   end;

type   
   TSub = class (TBase)
      class property Field1 : integer read Field write (Field);
      class property MultBy3 : Integer read (3*Field) write (Field := Value div 3);
      class property MultBy2 : Integer read (42*Field) write (Field := Value div 42);
   end;

type   
   TContainer = class
      class var FSub : TSub;
      class var FBase: TBase;
      class property FieldSub1 : integer read (FSub.Field) write (FSub.Field);
      class property MultBy2 : Integer read (FBase.MultBy2) write (FBase.MultBy2);
      class property MultByX : Integer read (TBase(FSub).MultBy2) write (TBase(FSub).MultBy2);
   end;

type
   TContainerRec = record
      Dummy : Integer;
      class var FBase: TBaseRec;
      class property MultBy2 : Integer read (FBase.MultBy2) write (FBase.MultBy2);
      class property MultBy3 : Integer read (FBase.Field1*3) write (FBase.Field1 := Value div 3);
   end;

var br : TBaseRec;

br.Field1 := 12;
PrintLn(br.Field);
PrintLn(br.Field1);
br.MultBy2 := 26;
PrintLn(br.Field);
PrintLn(br.Field1);

var cr : TContainerRec;

cr.MultBy2 := 30;
PrintLn(cr.MultBy2);
PrintLn(cr.FBase.MultBy2);
PrintLn(cr.FBase.Field);
cr.MultBy3 := 30;
PrintLn(cr.MultBy3);
PrintLn(cr.FBase.MultBy2);
PrintLn(cr.FBase.Field);

var b := new TBase;
b.Field1 := 12;
PrintLn(b.Field);
PrintLn(b.Field1);
b.MultBy2 := 26;
PrintLn(b.Field);
PrintLn(b.Field1);

var s := new TSub;
s.MultBy3 := 30;
PrintLn(s.Field1);
PrintLn(s.MultBy3);

var c := new TContainer;
c.FSub := s;
c.FBase := b;
c.MultBy2 := 42;
PrintLn(c.FBase.Field1);
c.MultByX := 42;
PrintLn(c.FBase.Field1);
