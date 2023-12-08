type TOuter = class
   private
      type TInner = class
         Field : Integer;
         procedure PrintMe;
      end;

      FInner : TInner;

      procedure PrintInner(obj : TInner);

   public
      constructor Create;

      procedure PrintIt;
end;


constructor TOuter.Create;
begin
   FInner := TInner.Create;
   FInner.Field := 12345;
end;

procedure TOuter.PrintIt;
begin
   PrintInner(FInner);
end;

procedure TOuter.TInner.PrintMe;
begin
   PrintLn(Field);
end;

procedure TOuter.PrintInner(obj : TInner);
begin
   obj.PrintMe;
end;

TOuter.Create.PrintIt;