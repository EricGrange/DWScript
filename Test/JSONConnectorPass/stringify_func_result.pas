function TestString : String; 
begin 
    Result:='123'; 
end;

function TestDynArray : array of Integer; 
begin 
    Result := [1,2,3]; 
end;

function TestStaticArray : array [2..3] of Integer; 
begin 
    Result := [4,5]; 
end;

function TestStaticArray1 : array [0..0] of Boolean; 
begin 
    Result := [True]; 
end;

function TestStaticArray2 : array [2..3] of array [0..1] of String; 
begin
    Result :=[['a','b'],['c','d']];
end;

PrintLn(JSON.Stringify(TestString));
PrintLn(JSON.Stringify(TestDynArray));
PrintLn(JSON.Stringify(TestStaticArray));
PrintLn(JSON.Stringify(TestStaticArray1));
PrintLn(JSON.Stringify(TestStaticArray2));
