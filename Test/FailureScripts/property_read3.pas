type  
  TFoo = class
  private
    FBar: TBar;
  public
    property Bar: TFoo read FBar;    
  end; 

var Test := TFoo.Create;
Test.Bar.