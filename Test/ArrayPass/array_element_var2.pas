type
    TRec = record 
        x, y : String;
        procedure Print; begin PrintLn(x + ',' + y); end;
    end;

var a : array of TRec;

procedure TestResize(var r : TRec);
begin
   var r2 := r;
   r.Print;
   r.x := 'bye';
   r.y := 'world';
   r.Print;
   r := r2;
   r.Print;
   a.Clear;
   a.SetLength(1);
   r.Print;
end;

a.SetLength(1);
a[0].x := 'hello';
a[0].y := 'World';
TestResize(a[0]);

procedure TestOutOfBound(var r : TRec);
begin
   a.Clear;
   try r.Print except on E : Exception do PrintLn('Read: ' + E.Message) end;
   try r.x := 'test'; except on E : Exception do PrintLn('Write X: ' + E.Message) end;
   try r.x := 'test'; except on E : Exception do PrintLn('Write Y: ' + E.Message) end;
   var r2 : TRec;
   try r := r2; except on E : Exception do PrintLn('Write XY: ' + E.Message) end;
end;

TestOutOfBound(a[0]);
