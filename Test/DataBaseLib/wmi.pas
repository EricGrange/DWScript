var db := new DataBase('WMI');
var ds := db.Query('select AddressWidth, CreationClassName from Win32_Processor');
PrintLn(ds.Fields[0].DeclaredType);
PrintLn(ds.Fields[0].DataType.Name);
PrintLn(ds.AsInteger(0) in [32,64]);
PrintLn(ds.AsString(1));