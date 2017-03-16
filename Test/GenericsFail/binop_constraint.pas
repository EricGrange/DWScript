type
    TTest<T> = class
       property Prop : T; 
       function Inc(v : T) : T;
       begin
          Prop := Prop + v;
          Result := Prop;
       end;
    end;
    
var ok1 := new TTest<Integer>;

var ko1 := new TTest<Boolean>;

var ko2 := new TTest<>;