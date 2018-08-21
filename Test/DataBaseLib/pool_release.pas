PrintLn(DataBasePool.Count('test'));

var db := DataBase.Create('GUID');

DataBasePool.Release('test', db);

PrintLn(DataBasePool.Count('test'));

if db<>nil then PrintLn('bug');

DataBasePool.Release('test', db, 0);

PrintLn(DataBasePool.Count('test'));

db := DataBase.Create('GUID');
DataBasePool.Release('test', db);

if DataBasePool.Acquire('test') <> nil then
    PrintLn('acquire test');
    
if DataBasePool.Acquire('bug') <> nil then
    PrintLn('acquire bug');
    
PrintLn(DataBasePool.Count('test'));

db := DataBase.Create('GUID');
DataBasePool.Release('test', db);

PrintLn(DataBasePool.Count('test'));

DataBasePool.Cleanup('z*');

PrintLn(DataBasePool.Count('test'));

DataBasePool.Cleanup('t*');

PrintLn(DataBasePool.Count('test'));

