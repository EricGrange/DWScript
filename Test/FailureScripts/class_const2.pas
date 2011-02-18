type TBase = class
   private const cPrivate = 1;
   protected const cProtected = 2;
end;
  
type TChild = class (TBase)
   procedure PrintFromChild;
end;

procedure TChild.PrintFromChild;
begin
   PrintLn(cProtected);
   PrintLn(cPrivate);
end;
