type 
    TRec<A,B> = record 
        FieldA : A;
        FieldB : B;
    end;
    
var r : TRec<Integer, String>;

r.FieldA := 123;
r.FieldB := 'abc';

var r2 := r;

PrintLn(r2.FieldA);
PrintLn(r2.FieldB);