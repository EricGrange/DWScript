type TTest1<T> = class
        procedure Dummy;
    end;

type TTest2<T, U> = class
    procedure Dummy;
end;

procedure TTest1<t>.Dummy; begin end;
procedure TTest1<u>.Dummy; begin end;

procedure TTest2<T>.Dummy; begin end;
procedure TTest2<T,v,w.Dummy; begin end;

