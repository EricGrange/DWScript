var x : Variant := DispCallProxy; // IDispatch assignment

var i : Integer;
try
   i := x; // into type (Integer)
except
   on E: Exception do PrintLn(E.Message);
end;

var s : String;
try
   s := x; // into type (String)
except
   on E: Exception do PrintLn(E.Message);
end;

var f : Float;
try
   f := x; // into type (Float)
except
   on E: Exception do PrintLn(E.Message);
end;

var b : Boolean;
try
   b := x; // into type (Boolean)
except
   on E: Exception do PrintLn(E.Message);
end;
