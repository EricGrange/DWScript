type TBase = class
   private const cPrivate = 1;
   protected const cProtected = 2;
   public class const cPublic = 3;
   procedure PrintAll;
   class procedure ClassPrintAll;
end;
  
type TChild = class (TBase)
   procedure PrintFromChild;
end;

procedure TBase.PrintAll;
begin
   PrintLn('From base');
   PrintLn(cPrivate);
   PrintLn(cProtected);
   PrintLn(cPublic);
end;

class procedure TBase.ClassPrintAll;
begin
   PrintLn('From class proc');
   PrintLn(cPrivate);
   PrintLn(cProtected);
   PrintLn(cPublic);
end;

procedure TChild.PrintFromChild;
begin
   PrintLn('From child');
   PrintLn(cProtected);
   PrintLn(cPublic);
end;

var o := TBase.Create;

PrintLn('From Outside');
PrintLn(TBase.cPublic);
PrintLn(o.cPublic);

TBase.ClassPrintAll;
o.PrintAll;

TChild.Create.PrintFromChild;





