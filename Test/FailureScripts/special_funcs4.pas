var i : Integer;

{$if Defined(123)}{$endif}

Inc(i, );

{$if Declared(IntToStr(i))}{$endif}
