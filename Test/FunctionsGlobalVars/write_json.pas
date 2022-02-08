var j := JSON.Serialize(record
	a := 'hello';
	b := [ 123, 456 ];
end);

CleanupGlobalVars;
WriteGlobalVar('json', j);

var v := ReadGlobalVarDef('json', 'bug');
PrintLn(VarIsStr(v));
PrintLn(v);
