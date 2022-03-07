
PrintLn(Base32Encoder.Decode('JBSWY3DPEBLW64TMMQ'));
PrintLn(Base32Encoder.Encode('Hello World'));

PrintLn(Base32Encoder.Decode(''));
PrintLn(Base32Encoder.Encode(''));

PrintLn(Base32Encoder.Encode(#0));

PrintLn(HexadecimalEncoder.Encode(Base32Encoder.Decode('000000')));
PrintLn(Base32Encoder.Encode(#0#0#0#0#0#0#0));

PrintLn(HexadecimalEncoder.Encode(Base32Encoder.Decode('AAAQSZTXMADJKPKVM5BZ4XRZ7BVA2JZ353LBSZ7W')));
PrintLn(Base32Encoder.Encode(HexadecimalEncoder.Decode('00010966776006953D5567439E5E39F86A0D273BEED61967F6')));

try
   PrintLn(Base32Encoder.Decode('....'));
except
   on E: Exception do
      PrintLn(E.Message);
end;



