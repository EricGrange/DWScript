function Test(o : TObject) : TObject;
begin
   Result:=o;
end;

function TestNil : TObject;
begin
   Result:=nil;
end;

if not Assigned(TObject.Create) then
   PrintLn('bug 1');

if not Assigned(Test(TObject.Create)) then
   PrintLn('bug 2');

if Assigned(Test(nil)) then
   PrintLn('bug 3');

if Assigned(TestNil) then
   PrintLn('bug 4');

if Assigned(ExceptObject) then
   PrintLn('bug 5');
