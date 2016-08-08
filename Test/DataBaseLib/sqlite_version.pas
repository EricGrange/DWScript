var db := DataBase.Create('SQLite');

PrintLn(db.VersionInfoText.Matches('3.*.*'));
