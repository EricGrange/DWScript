var s := 'a string';
var jv := JSON.Parse(s) ?? JSON.NewObject;
PrintLn(jv.Toto); // should pass because jv is a JSONVariant
var v := JSON.Parse(s) ?? 1;
PrintLn(v.Toto); // should fail because v is a Variant
