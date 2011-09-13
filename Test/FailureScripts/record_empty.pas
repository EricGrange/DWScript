type 
   TRec = record
      function Hello : String;
      begin
         Result:='Hello';
      end;
      property PropHello : Integer read Hello;
   end;
   
var r : TRec;

r.Bug;