procedure Test1(p : array [String] of String);
begin
    Print('Test1: ');
    PrintLn(p.Keys.Join(','));
end;

procedure Test2(const p : array [String] of String);
begin
    Print('Test2: ');
    PrintLn(p.Keys.Join(','));
end;

procedure Test3(var p : array [String] of String);
begin
    Print('Test3: ');
    PrintLn(p.Keys.Join(','));
    var loc : array [String] of String;
    loc['gamma'] := 'g';
    p := loc;
end;


var a : array [String] of String;
a['alpha'] := 'a';
a['beta'] := 'b';

Test1(a);
Test2(a);
Test3(a);
Test1(a);

