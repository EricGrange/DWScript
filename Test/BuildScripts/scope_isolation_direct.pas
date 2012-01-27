unit scope_isolation_direct;

interface

uses scope_isolation_indirect;

procedure ShouldWork;

implementation

procedure ShouldWork;
begin
   //ShouldFail;
end;

end.