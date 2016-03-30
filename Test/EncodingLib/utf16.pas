
procedure Test(e : class of Encoder; s : String);
begin
	var u := e.Encode(s);
	PrintLn(HexadecimalEncoder.Encode(u));
	PrintLn(e.Decode(u));
end;

Test(UTF16BigEndianEncoder, 'Example');
Test(UTF16LittleEndianEncoder, 'Example');

Test(UTF16BigEndianEncoder, 'éric');
Test(UTF16LittleEndianEncoder, 'éric');
