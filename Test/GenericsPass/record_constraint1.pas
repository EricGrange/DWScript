type TRec<T: record> = record 
        Sub : T;
    end;
    
type TPoint = record
        X, Y : Integer;
    end;
    
type TRecPoint = TRec<TPoint>;
    
type TRecSubPoint = TRec<TRecPoint>;

var rp : TRecPoint;
rp.Sub.X := 1;
rp.Sub.Y := 2;

var rsp : TRec<TRecPoint>;
rsp.Sub := rp;

PrintLn(rsp.Sub.Sub.X);
PrintLn(rsp.Sub.Sub.Y);