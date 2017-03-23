type TRec<T: record> = record 
        Sub : T;
    end;
    
var a : TRec<Integer>;
var b : TRec<TObject>;
var c : TRec<nil>;
