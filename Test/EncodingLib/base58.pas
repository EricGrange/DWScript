
PrintLn(Base58Encoder.Decode('JxF12TrwUP45BMd'));
PrintLn(Base58Encoder.Encode('Hello World'));

PrintLn(Base58Encoder.Decode(''));
PrintLn(Base58Encoder.Encode(''));

PrintLn(HexadecimalEncoder.Encode(Base58Encoder.Decode('1')));
PrintLn(Base58Encoder.Encode(#0));

PrintLn(HexadecimalEncoder.Encode(Base58Encoder.Decode('1111111')));
PrintLn(Base58Encoder.Encode(#0#0#0#0#0#0#0));

PrintLn(HexadecimalEncoder.Encode(Base58Encoder.Decode('16UwLL9Risc3QfPqBUvKofHmBQ7wMtjvM')));
PrintLn(Base58Encoder.Encode(HexadecimalEncoder.Decode('00010966776006953D5567439E5E39F86A0D273BEED61967F6')));

try
   PrintLn(Base58Encoder.Decode('....'));
except
   on E: Exception do
      PrintLn(E.Message);
end;



