type
   TMyObj = class
      function GetSelf : TMyObj;
   end;

function TMyObj.GetSelf : TMyObj;
begin
   Result:=Self;
end;

var o = TMyObj.Create;

if o<>o.GetSelf then PrintLn('bug');


