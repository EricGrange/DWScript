type
   TBase = class end;
type
   TSub = class (TBase);
type
   TThird = class (TObject);
type
   TSubThird = class (TThird);
   
procedure DoIt(p : array of TObject); overload;
begin
   PrintLn('objects');
end;
   
procedure DoIt(p : array of TSub); overload;
begin
   PrintLn('subs');
end;

procedure DoIt(p : array of TThird); overload;
begin
   PrintLn('thirds');
end;

var o := TObject.Create;
var b := TBase.Create;
var s := TSub.Create;
var t := TThird.Create;
var st := TSubThird.Create;

DoIt([o]);
DoIt([b]);
DoIt([s]);
DoIt([t]);
DoIt([st]);

DoIt([o, b]);
DoIt([b, s]);
DoIt([s, o]);
DoIt([s, t]);
DoIt([t, s]);
DoIt([t, st]);
DoIt([st, t]);
