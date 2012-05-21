type
   TRec = record
      Field : Integer = 1;
      Name := 'hello';
      procedure PrintMe; 
      begin 
         PrintLn(Field);
         PrintLn(Name);
      end;
   end;

var r : TRec;

r.PrintMe;

r.Field:=2;
r.Name:='world';
r.PrintMe;

const cr1 : TRec = (Field : 123);
const cr2 : TRec = (Name : 'World');

cr1.PrintMe;
cr2.PrintMe;

