type TProc = procedure;

type TMyFunc = function : String;

type TMyFuncOne = function (i : Integer) : String;

type 
   TTest = class
      FProc : TProc;
      FMyFunc : TMyFunc;
      FMyFuncOne : TMyFuncOne;
      
      property Proc : TProc read FProc;
      property MyFunc : TMyFunc read FMyFunc;
      property MyFuncOne : TMyFuncOne read FMyFuncOne;
   end;

var t := new TTest;

procedure Hello;
begin
   PrintLn('Hello');
end;

function World : String;
begin
   Result:='World';
end;

t.FProc:=Hello;
t.FMyFunc:=World;
t.FMyFuncOne:=IntToStr;

t.Proc();
PrintLn(t.MyFunc());

t.Proc;
PrintLn(t.MyFunc);
PrintLn(t.MyFuncOne(1+1));



