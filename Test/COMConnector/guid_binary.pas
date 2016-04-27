uses COM, System.Encoding;

var guid := '{E0A4F576-CC88-4818-B8CA-D4B746E06D2B}';
var b := GUIDToBinary(guid); 
PrintLn(HexadecimalEncoder.Encode(b));
PrintLn(BinaryToGUID(b));
