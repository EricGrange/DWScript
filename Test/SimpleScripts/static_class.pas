type
   TStatic = class static
      const MyConst = 'hello';
      class procedure PrintMe; virtual;
      begin
         PrintLn('world');
      end;
   end;

type TStaticClass = class of TStatic;

type
   TSubStatic = class (TStatic)
      class procedure PrintMe; override;
      begin
         Print(MyConst+' ');
         inherited;
      end;
   end;

PrintLn('const = '+TStatic.MyConst);
TStatic.PrintMe;
TSubStatic.PrintMe;

var c : TStaticClass;

c:=TStatic;
c.PrintMe;

c:=TSubStatic;
c.PrintMe;
