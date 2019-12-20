FormatSettings.Zone := DateTimeZone.UTC;
FormatSettings.ShortDateFormat := 'dd mmm yyyy';

var ADate := StrToDateTime('18 Dec 2019 17:09:09');

PrintLn(FormatDateTime('dd.mm.yyyy hh:nn:ss', ADate)); 