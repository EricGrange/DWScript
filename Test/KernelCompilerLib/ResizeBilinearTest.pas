var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var rs_def := k.AddResizeBilinear(in1, 4, 4);
var rs_ac := k.AddResizeBilinear(in1, 4, 4, True, False);
var rs_hpc := k.AddResizeBilinear(in1, 4, 4, False, True);
var rs_both := k.AddResizeBilinear(in1, 4, 4, True, True);

k.MarkOutput(rs_def);
k.MarkOutput(rs_ac);
k.MarkOutput(rs_hpc);
k.MarkOutput(rs_both);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 1]);
var b_out_def := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var b_out_ac := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var b_out_hpc := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var b_out_both := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);

// 2x2 grid:
// 10  20
// 30  40
b_in.SetData([0, 0, 0], 10);
b_in.SetData([0, 1, 0], 20);
b_in.SetData([1, 0, 0], 30);
b_in.SetData([1, 1, 0], 40);

TKCLKernelCompiler.Dispatch(k, [b_in, b_out_def, b_out_ac, b_out_hpc, b_out_both]);

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 1000) / 1000;
   Result := FloatToStr(rv);
end;

procedure PrintBuf(title : String; b : TKCLStridedBuffer);
begin
   PrintLn(title);
   for var y := 0 to 3 do begin
      for var x := 0 to 3 do
         Print(RoundedStr(b.GetData([y, x, 0])) + ' ');
      PrintLn('');
   end;
end;

PrintBuf('-- Default --', b_out_def);
PrintBuf('-- AlignCorners --', b_out_ac);
PrintBuf('-- HalfPixelCenters --', b_out_hpc);
PrintBuf('-- Both --', b_out_both);
