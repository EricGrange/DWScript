type
   TRec = record
      Field  : Integer;
      function Test : String; overload;
      begin
         Result:=IntToStr(Field);
      end;
      function Test(a : Integer) : String; overload;
      begin
         Result:=IntToStr(Field+a);
      end;
   end;
   
type
   TCls = class
      function Test : String; overload;
      begin
         Result:='Test';
      end;
      function Test(a : String) : String; overload;
      begin
         Result:='Test: '+a;
      end;      
   end;
   
   var r : TRec;
   r.Field:=2;
   PrintLn(r.Test);
   PrintLn(r.Test(10));
   
   var o := TCls.Create;
   PrintLn(o.Test);
   PrintLn(o.Test('Hello'));