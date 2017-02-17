procedure log(data : Variant); overload; begin end;
procedure log(const data : array of const); overload; begin end;

procedure cwd; begin end;

log(cwd);
log(1);
log(@cwd);
