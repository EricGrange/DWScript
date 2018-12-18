procedure TestProc(paramProc : procedure); overload; forward;
procedure TestProc(paramProc : procedure(param : Integer)); overload; forward;
procedure TestProc(paramProc : procedure(param : String)); overload; forward;

procedure TestFunc(paramFunc : function : Integer); overload; forward;
procedure TestFunc(paramFunc : function(param : Integer) : Integer); overload; forward;
procedure TestFunc(paramFunc : function(param : Integer) : String); overload; forward;

procedure TestProc(paramProc : procedure); overload;
begin
    Print('proc: ');
    paramProc;
end;

procedure TestProc(paramProc : procedure(param : Integer)); overload;
begin
    Print('proc_Int: ');
    paramProc(123);
end;

procedure TestProc(paramProc : procedure(param : String)); overload;
begin
    Print('proc_Str: ');
    paramProc('abc');
end;


procedure TestFunc(paramFunc : function : Integer); overload;
begin
    Print('func: ');
    PrintLn(paramFunc);
end;

procedure TestFunc(paramFunc : function(param : Integer) : Integer); overload;
begin
    Print('func_Int: ');
    PrintLn(paramFunc(456));
end;

procedure TestFunc(paramFunc : function(param : Integer) : String); overload;
begin
    Print('func_Str: ');
    PrintLn(paramFunc(789));
end;

procedure Hello; begin PrintLn('hello'); end;
procedure HelloInt(p : Integer); begin PrintLn(p*2); end;
procedure HelloStr(s : String); begin PrintLn(s+s); end;

TestProc(@Hello);
TestProc(@HelloInt);
TestProc(@HelloStr);

function World : Integer; begin Result := 314; end;
function WorldInt(i : Integer) : Integer; begin Result := i*11; end;
function WorldStr(i : Integer) : String; begin Result := i.ToHexString(8); end;

TestFunc(@World);
TestFunc(@WorldInt);
TestFunc(@WorldStr);

