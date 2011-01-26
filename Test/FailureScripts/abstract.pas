type 
   TTest1 = class abstract
   end;
type
   TTest2 = class
      procedure Dummy; virtual; abstract;
   end;
type
   TTest3 = class (TTest1)
   end;
type
   TTest4 = class (TTest2)
      procedure Dummy; override;
   end;
type
   TTest5 = class (TTest2)
      procedure Dummy; reintroduce;
   end;

procedure TTest4.Dummy;
begin
end;

procedure TTest5.Dummy;
begin
end;

TTest1.Create;
TTest2.Create;
TTest3.Create;
TTest4.Create;
TTest5.Create;