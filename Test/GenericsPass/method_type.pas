type 
    TMy<T> = class 
        class function Test(p : T) : T; static; begin Result := p; end;
    end;

var i : Integer := TMy<Integer>.Test(123);
PrintLn(i);

var s : String  := TMy<String>.Test('abc');
PrintLn(s);