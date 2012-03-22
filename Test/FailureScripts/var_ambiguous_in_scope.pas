var test : Integer;

procedure Proc1(test : Integer);
var
   test : String;
begin
end;

procedure Proc2(test : Integer);
begin
   var test : String;
end;

procedure Proc3;
var
   test : Integer;
begin
   begin
      var test : String;
   end;
end;

procedure Proc4;
var
   test : Integer;
begin
   procedure Proc5;
   var
      test : Integer;
   begin
      begin
         var test : Integer;
      end;
   end;
end;
