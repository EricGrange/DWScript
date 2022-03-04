type
    TRec = record 
        x, y : String;
        procedure Print; begin PrintLn(x + ',' + y); end;
    end;

var a : array of TRec;

procedure SubTest(var v : String);
begin
   PrintLn(v);
   v := 'dws';
   a[0].Print;
   a.Clear;
   try PrintLn(v); except on E : Exception do PrintLn('v, ' + E.Message) end;
end;

procedure TestResize(var r : TRec);
begin
   SubTest(r.y);
   try r.Print; except on E : Exception do PrintLn('r, ' + E.Message) end;
end;

a.SetLength(1);
a[0].x := 'hello';
a[0].y := 'World';
TestResize(a[0]);

