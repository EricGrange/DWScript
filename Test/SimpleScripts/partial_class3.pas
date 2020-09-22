type TTest = partial class
  function S : String; begin exit('toto') end;
end;

type TTest = class
  private
   FS : String = 'test';
  public
   function T : String; begin Result := FS end;
end;

var t := new TTest;
PrintLn(t.S);
PrintLn(t.T);