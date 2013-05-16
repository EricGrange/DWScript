type
 TOnGetValue = function : string of object;

type
  TTestClass = class
   private
    fOnGetValue: TOnGetValue;
   public
    function Get : string;
    property OnGetValue: TOnGetValue read fOnGetValue write fOnGetValue;
  end;

function TTestClass.Get: string;
begin
 if Assigned(fOnGetValue) then
  Result := fOnGetValue   //LINE WITH COMPILE ERROR!
 else
  Result := 'event not assigned';
end;
type
TMainClass = class
 function DoIt : string;
 function GetValue : string;
end;

function TMainClass.GetValue : string;
begin
 result := 'passed';
end;

function TMainClass.DoIt : string;
begin
 var Test : TTestClass = TTestClass.Create;
 try
 Test.OnGetValue := GetValue;
 result := Test.Get;
 finally
 Test.free
 end;
end;

begin
 var M : TMainClass = TMainClass.Create;
 PrintLn(M.DoIt);
end;