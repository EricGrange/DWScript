uses System.Encoding;

var pixmap : TPixmap;

pixmap.AssignHexString('00000000');

var r := PixmapToPNGData(pixmap, 1, 1);

PrintLn(Base64Encoder.Encode(r));

r := PixmapToPNGData(pixmap, 1, 1, True);

PrintLn(Base64Encoder.Encode(r));
