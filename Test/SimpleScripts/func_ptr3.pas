type
   TMyFunc = function (s : String) : String;

type
   TMyRec = record
      Proc1 : TMyFunc;
      Proc2 : TMyFunc;
   end;

type
   TMyClass = class

      FRec : TMyRec;
      FAFunc : TMyFunc;

      procedure Print;
      
      function GetProc1 : TMyFunc;
      procedure SetProc1(f : TMyFunc);
      
      property AFunc : TMyFunc read FAFunc write FAFunc;
      
   end;

procedure TMyClass.Print;
begin
   PrintLn(FRec.Proc1('World'));
   PrintLn(FRec.Proc2('world!'));
   PrintLn(FAFunc('world'));
end;

function TMyClass.GetProc1 : TMyFunc;
begin
   Result:=FRec.Proc1;
end;

procedure TMyClass.SetProc1(f : TMyFunc);
begin
   FRec.Proc1:=f;
end;
  
function Func1(s : String) : String;
begin
   Result:='Hello '+s;
end;

function Func2(str : String) : String;
begin
   Result:='ByeBye '+str;
end;

function Func3(str : String) : String;
begin
   Result:='Ho ho ho '+str;
end;

var o := TMyClass.Create;

o.FRec.Proc1:=Func1;
o.FRec.Proc2:=Func2;
o.AFunc := Func3;

o.Print;

PrintLn(o.GetProc1()('get'));

PrintLn(o.AFunc('direct prop'));

var old : TMyFunc := o.GetProc1();
o.SetProc1(Func2);

PrintLn(old('old'));
old := o.FRec.Proc1;
PrintLn(old('new'));
