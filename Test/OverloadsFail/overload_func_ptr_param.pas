procedure TestProc(paramProc : procedure); overload; forward;
procedure TestProc(paramProc : procedure(param : Integer)); overload; forward;
procedure TestProc(paramProc : procedure(param : String)); overload; forward;

procedure TestFunc(paramFunc : function : Integer); overload; forward;
procedure TestFunc(paramFunc : function(param : Integer) : Integer); overload; forward;
procedure TestFunc(paramFunc : function(param : Integer) : String); overload; forward;

procedure TestProc(paramProc : procedure); overload;
begin
end;

procedure TestProc(paramProc : procedure(param : Integer)); overload;
begin
end;

procedure TestFunc(paramFunc : function : Integer); overload;
begin
end;

procedure TestFunc(paramFunc : function(param : Integer) : String); overload;
begin
end;
