// JWT  taken from jwt.io samples

const cToken = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWUsImlhdCI6MTUxNjIzOTAyMn0.POstGetfAytaZS82wHcjoTyoqhMyxXiWdR7Nn7A29DNSl0EiXLdwJ6xC6AfgZWF1bOsS_TuYI3OG85AmiExREkrS6tDfTQ2B3WXlrr-wp5AokiRbz3_oB4OxG-W9KcEEbDRcZc0nH3L7LzYptiy1PtAylQGxHTWZXtGz4ht0bAecBgmpdgXMguEIcoqPJ1n3pIWk_dUZegpqx0Lka21H6XxUTxiy8OcaarA8zdnPUnV6AmNP3ecFawIFYdvJB_cm-GvpCSbr8G8y_Mllj8f4x9nBH8pQux89_6gUY618iYv7tuPWBFfEbLxtF2pZS6YC1aSfLQxeNe8djT9YjpvRZA';

var header := cToken.Before('.');
var payload := cToken.Between('.', '.');
var signature := cToken.AfterLast('.');

const cKeyInfo = #'
   {
   "size": 2048,
   "exponent": "010001",
   "modulus": "9f3ca2b356637cd0746c180a14c4afbe44edc25bc1b1a26aed2e7003e933795395a555b091675585ae2cdfc5ccfe96bfcabe3b6afefecf75539af0d1c801dc693f76c214441692eff5c8f99537894a26f2aff32b9bf62d8c26555a068e608870ad7c0a2ea3ebff5d629d6b0091f232b6f1d64f165811c5cb8005c5b94b9a4b7b85f60122350c33193535bf416a92a4c1af807c9d6dc708de3b5d4bb4b7c6347be95fe2ce0ec506b0583efd27dff9777472d2f6d5dc09b516d189889bcec11b087d50a10e9612b537074c232ab6f59b57a2f5d415a4a73197496e07bf8dea6be19260e0f6414ebc31be7da12936381f81b4e2e92687e66c610682f9b0c8223d33"
   }';
var key := TRSAKey.ImportJSON(cKeyInfo);
var hash := HashSHA256.HashData(header + '.' + payload);

PrintLn(key.VerifyHash('SHA256', hash, Base64URIEncoder.Decode(signature)));
PrintLn(key.VerifyHash('SHA256', HashSHA256.HashData(header + '_' + payload), Base64URIEncoder.Decode(signature)));
