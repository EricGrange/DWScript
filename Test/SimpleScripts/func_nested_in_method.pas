type
   tt=class
      private
         function a : String;
         begin
            Result:='a';
         end;

      public
         procedure Test1;
         begin
            function SubTest1 : String;
            begin
               var t := Self;
               Result:=t.a;
            end;
            var s:=SubTest1;
            PrintLn(s);
         end;
         
         procedure Test2;
         begin
            function SubTest2 : String;
            begin
               Result:=a;
            end;
            PrintLn(SubTest2);
         end;
         
         procedure Test3;
         begin
            function SubTest3 : String;
            begin
               Result:=Self.a;
            end;
            PrintLn(SubTest3);
         end;
         
   end;

var tst:=tt.Create;
tst.Test1;
tst.Test2;
tst.Test3;