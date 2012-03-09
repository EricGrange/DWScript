Type TObj = Class

 Procedure CreateEx(S : String); Overload; 
 Procedure CreateEx(I : Integer); Overload;
 
End;

Procedure TObj.CreateEx(S : String);
Begin
   PrintLn('String '+S);
End;

Procedure TObj.CreateEx(I : Integer);
Begin
   PrintLn('Integer '+IntToStr(I));
End;

var o := new TObj;
o.CreateEx('hello');
o.CreateEx(123);

TObj(nil).CreateEx('world');
TObj(nil).CreateEx(456);