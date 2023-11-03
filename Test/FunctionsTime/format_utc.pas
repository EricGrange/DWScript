
var d := Now;

var s := FormatDateTime('yyyy-mm-dd (uuu)', d);

PrintLn(StrMatches(s, '????-??-?? (UTC?*)'));

// attempt summer / winter
var d1 := EncodeDate(2023, 7, 1);
var d2 := EncodeDate(2023, 12, 1);

if (LocalDateTimeToUTCDateTime(d1) - d1) <> (LocalDateTimeToUTCDateTime(d2) - d2) then begin

	PrintLn(FormatDateTime('uuu', d1) <> FormatDateTime('uuu', d2));

end else begin

	PrintLn(FormatDateTime('uuu', d1) = FormatDateTime('uuu', d2));

end;

