type
   TBase = class end;
type
   TChild = class(TBase) end;

var cond : Boolean;
var base := new TBase;
var child := new TChild;

PrintLn((if cond then TBase else TChild).ClassName);
PrintLn((if cond then base else child).ClassName);
PrintLn(if cond then 1.5 else 2);

PrintLn((if cond then TChild else TBase).ClassName);
PrintLn((if cond then child else base).ClassName);
PrintLn(if cond then 2 else 1.5);

cond:=not cond;

PrintLn((if cond then TBase else TChild).ClassName);
PrintLn((if cond then base else child).ClassName);
PrintLn(if cond then 1.5 else 2);

PrintLn((if cond then TChild else TBase).ClassName);
PrintLn((if cond then child else base).ClassName);
PrintLn(if cond then 2 else 1.5);
