 type
  TPersistent = class
    fString:string;
    function GetOwner:TPersistent;
    function GetNamePath: String;
  end;

  function TPersistent.GetOwner: TPersistent;
  begin
    result:= nil;
  end;

  function TPersistent.GetNamePath: string;
  var
    S:String;
  begin
    Result:= ClassName;
    if (GetOwner <> nil) then
    begin
      s:=GetOwner.GetNamePath;
      if s <> '' then
        Result:=S+'.'+Result;
    end;
  end;
  
var p := TPersistent.Create;
PrintLn(p.GetNamePath);