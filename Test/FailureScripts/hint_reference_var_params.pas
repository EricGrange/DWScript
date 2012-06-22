
type

   TTest = class

      procedure Test1(var o : TObject);
      begin
         //
      end;

      procedure Test2(var o : TObject); virtual;
      begin
         //
      end;
      
      procedure Test3(const o : TObject); virtual;
      begin
         //
      end;
      
     end;

procedure Test1(var o : TObject);
begin
   //
end;

procedure Test2(var o : TObject);
begin
   o:=TObject.Create;
end;

procedure Test3(const o : TObject);
begin
   o.Free;
end;