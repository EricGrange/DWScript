var b : Boolean;

var o1 := if b then TObject.Create else nil;
var o2 := if b then nil else TObject.Create;

PrintLn(if Assigned(o1) then o1.ClassName else 'nil');
PrintLn(if Assigned(o2) then o2.ClassName else 'nil');