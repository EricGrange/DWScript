type 
    TBase = class
        constructor Create(i : Integer); overload;
        begin 
            PrintLn('Integer '+IntToStr(i)); 
        end;
        constructor Create(s : String); overload;
        begin 
            PrintLn('String '+s); 
        end;
    end;

type
   TTest = class (TBase)
      constructor CreateInteger;
      begin 
         inherited Create(123);
      end;
      constructor CreateString;
      begin 
         inherited Create('hello');
      end;
   end;

TTest.CreateInteger;
TTest.CreateString;