var a : array of String = nil;
var b : array of String = nil;

// a = nil
Print('a = nil: '); PrintLn(a = nil);
Print('nil = a: '); PrintLn(nil = a);

// a = b (both nil/empty)
Print('a = b (empty): '); PrintLn(a = b);

a := new String[1];
// a = nil (a is not nil)
Print('a[1] = nil: '); PrintLn(a = nil);
Print('nil = a[1]: '); PrintLn(nil = a);

// a = b (a is not nil, b is nil)
Print('a[1] = b(empty): '); PrintLn(a = b);

b := new String[1];
// a = b (both not nil, different instances)
Print('a[1] = b[1] (diff): '); PrintLn(a = b);

var c := a;
// a = c (both not nil, same instance)
Print('a[1] = c (same): '); PrintLn(a = c);

a := nil;
// a = b (a is nil, b is not nil)
Print('a(empty) = b[1]: '); PrintLn(a = b);

b := nil;
// a = b (both nil)
Print('a = b (both empty): '); PrintLn(a = b);
