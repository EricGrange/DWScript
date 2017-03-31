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

function TestObj : TObject;
begin
    Result := new TObject;
end;

function TestObjNil : TObject;
begin
    Result := nil;
end;

type
    TObject2 = class
        Hello := 'World';
    end;
function TestObj2 : TObject;
begin
    Result := new TObject2;
end;

PrintLn(JSON.Stringify(TestString));
PrintLn(JSON.Stringify(TestDynArray));
PrintLn(JSON.Stringify(TestStaticArray));
PrintLn(JSON.Stringify(TestStaticArray1));
PrintLn(JSON.Stringify(TestStaticArray2));
PrintLn(JSON.Stringify(TestObj));
PrintLn(JSON.Stringify(TestObjNil));
PrintLn(JSON.Stringify(TestObj2));
