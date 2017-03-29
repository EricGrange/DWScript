type 
    TRec = record
        private
            FTest : Integer = 123;
        published 
            property Test : Integer read FTest;
            property Test2 : Integer external 'hello' read FTest;
    end;

var r : TRec;
PrintLn(JSON.Stringify(r));
