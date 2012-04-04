type
  TObjectArr = array of TObject;

type  
  TTest = class
    class procedure Client(controls: TObjectArr); overload;
    class procedure Client(control: TObject); overload;
  end;

class procedure TTest.Client(controls: TObjectArr);
begin
   Print('Array :');
   PrintLn(controls.Length);
end;

class procedure TTest.Client(control: TObject);
begin
   PrintLn('Object');
end;

var o : TObject;

TTest.Client(o);
TTest.Client([o]);
TTest.Client([o, o]);

TTest.Client(nil);
TTest.Client([]);
TTest.Client([nil]);
TTest.Client([nil, nil]);


