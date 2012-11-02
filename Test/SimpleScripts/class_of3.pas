type
  TMyControl = TObject;
type
  TMyControlClass = class of TMyControl;
type
   TMySubControl = class (TMyControl);
  
PrintLn(TMyControl.ClassName);
PrintLn(TMyControlClass.ClassName);
PrintLn(TMySubControl.ClassName);