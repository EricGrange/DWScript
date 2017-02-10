type 
    TTest<A,B> = class 
        FieldA : A;
        FieldB : B;
    end;
    
var r := new TTest<Integer, String>;

PrintLn(r.ClassName);

r.FieldA := 123;
r.FieldB := 'abc';

var r2 := r;

PrintLn(r2.FieldA);
PrintLn(r2.FieldB);