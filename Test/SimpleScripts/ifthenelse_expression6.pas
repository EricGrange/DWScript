var a, b, c : array of String;

a := []; 
b := ['1', '2', '3']; 

PrintLn(a.Join(','));
PrintLn(b.Join(','));

c := if a.Count > 0 then b else a; 
PrintLn(c.Join(','));
c := if a.Count = 0 then b else a; 
PrintLn(c.Join(','));

c := if a.Count > 0 then a else b; 
PrintLn(c.Join(','));
c := if a.Count = 0 then a else b; 
PrintLn(c.Join(','));

c := if a.Count > 0 then b else []; 
PrintLn(c.Join(','));
c := if a.Count = 0 then b else []; 
PrintLn(c.Join(','));

c := if a.Count > 0 then [] else b; 
PrintLn(c.Join(','));
c := if a.Count = 0 then [] else b; 
PrintLn(c.Join(','));

PrintLn("done");
