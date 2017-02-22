type 
    TTest<T> = class 
        Field1, Field2 : T;
        property Prop12 : T read Field1 write Field2;
        property Prop21 : T read Field2 write Field1;
    end;
    
var r := new TTest<Integer>;

r.Prop12 := 2;
r.Prop21 := 1;

PrintLn(r.Field1);
PrintLn(r.Prop12);
PrintLn(r.Field2);
PrintLn(r.Prop21);