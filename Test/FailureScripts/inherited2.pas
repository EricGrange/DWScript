type
   TBase = class
      procedure Proc1;
      procedure Proc2; virtual; abstract;
   end;
   
type
   TChild = class(TBase)
      procedure Proc2; override;
      procedure Proc3;
   end;   
   
procedure TBase.Proc1;
begin
   PrintLn(inherited ClassName);
end;

procedure TChild.Proc2;
begin
   inherited;
end;

procedure TChild.Proc3;
begin
   inherited;
end;