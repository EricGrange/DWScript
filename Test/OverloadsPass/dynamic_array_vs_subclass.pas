type TTest = class;

type TObjectArr = array of TObject;

type TTestArr = array of TTest;

type  
  TTest = class
    class procedure Client(controls: TObjectArr); overload;
    class procedure Client(control: TObject); overload;
    class procedure Client(controls: TTestArr); overload;
  end;

class procedure TTest.Client(controls: TObjectArr);
begin
   Print('Object Array :');
   PrintLn(controls.Length);
end;

class procedure TTest.Client(controls: TTestArr);
begin
   Print('Test Array :');
   PrintLn(controls.Length);
end;

class procedure TTest.Client(control: TObject);
begin
   PrintLn('Object');
end;

var t : TTest;
var o : TObject;

TTest.Client(o);
TTest.Client([o]);
TTest.Client([o, o]);

TTest.Client(t);
TTest.Client([t]);
TTest.Client([t, t]);


