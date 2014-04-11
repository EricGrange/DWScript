PrintLn(DataBasePool.Count('test'));

var db := DataBase.Create('GUID');

DataBasePool.Release('test', db);

PrintLn(DataBasePool.Count('test'));

if db<>nil then PrintLn('bug');

DataBasePool.Release('test', db, 0);

PrintLn(DataBasePool.Count('test'));
