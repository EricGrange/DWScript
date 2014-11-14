uses System.Encoding;

var pixmap : array of Integer;

pixmap.Add(0);

var r := PixmapToPNGData(pixmap, 1, 1);

PrintLn(Base64Encoder.Encode(r));

r := PixmapToPNGData(pixmap, 1, 1, True);

PrintLn(Base64Encoder.Encode(r));