unit test;

interface

type
   TTest1 = class
      protected
         Field : Integer;
   end;

implementation

type
   TTest2 = class
      private
         procedure Internal;
         begin
         end;
   end;

   TTest3 = class
      Field1: TTest1;
      Field2: TTest2;
      procedure Here;
   end;

procedure TTest3.Here;
begin
   Print(IntToStr(Field1.Field));
   Field2.Internal;
end;

end.