type
   TMy = class (TObject)
      function ToStr : String; virtual; begin Result := 'hello'; end;
   end;

type
   TMy2 = class (TMy)
      function ToStr : String; override; begin Result := 'world'; end;
   end;

var a : array of TMy;

a.Add(new TMy, new TMy2);

PrintLn(a.Map(lambda (e) => e.ToStr).Join(','));

a.Swap(0, 1);

PrintLn(a.Map(lambda (e) => e.ToStr).Join(','));

PrintLn(a.IndexOf(nil));
PrintLn(a[1] in a);

a.Add(a);

PrintLn(a.Map(lambda (e) => e.ToStr).Join(','));

PrintLn(a.IndexOf(a[2]));
