type
  TTest = class
  private
    F: Variant;
  public
    constructor Create(r: string; flags: string = '');
    function Exec(s: string): Integer; overload;
    function Match(s: string): Integer; overload;
    function Match(s: string; var idx: integer): Integer; overload;
    class function Match(s, r: string; flags: string = ''): Integer; overload;
    class function Match(s: string; var idx: integer; r: string; flags: string = ''): Integer; overload;
end;

procedure Stuff;
var
   v : Integer;
   r : TTest;
begin
   r.Match('n', v);
end;