var k := TKCLKernel.Create;
var in_layer := k.AddInput('in');

// 1. Depthwise Conv 3x3, stride 1 (Channels: 2)
var dw_weights : array of Float;
dw_weights.SetLength(3 * 3 * 2);
for var i := 0 to dw_weights.Length - 1 do dw_weights[i] := 0.5;
var dw_bias : array of Float = [0.1, -0.1];
var dw := k.AddDepthwiseConv2D(in_layer, dw_weights, dw_bias, 3, 1);

// 2. HardSwish activation
var hs := k.AddHardSwish(dw);

// 3. Pointwise Conv 1x1, 2 channels -> 2 channels
var pw_weights : array of Float;
pw_weights.SetLength(1 * 1 * 2 * 2);
pw_weights[0] := 1.0; pw_weights[1] := 0.0;
pw_weights[2] := 0.0; pw_weights[3] := 1.0;
var pw_bias : array of Float = [0.5, -0.5];
var pw := k.AddConv2D(hs, pw_weights, pw_bias, 1, 1);

// 4. Squeeze and Excite (GlobalAvgPool -> Conv2D -> HardSigmoid -> Multiply)
var se_pool := k.AddGlobalAvgPool(pw);

var se_weights : array of Float;
se_weights.SetLength(1 * 1 * 2 * 2);
se_weights[0] := 1.0; se_weights[1] := 0.0;
se_weights[2] := 0.0; se_weights[3] := 1.0;
var se_bias : array of Float = [0.0, 0.0];
var se_conv := k.AddConv2D(se_pool, se_weights, se_bias, 1, 1);

var se_sig := k.AddHardSigmoid(se_conv);
var se_scaled := k.AddMul(pw, se_sig); 

// 5. Residual connection
var out_node := k.AddAdd(in_layer, se_scaled);
k.MarkOutput(out_node);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);

// Initialize input
for var y := 0 to 3 do
   for var x := 0 to 3 do begin
      b_in.SetData([y, x, 0], 1.0);
      b_in.SetData([y, x, 1], -1.0);
   end;

TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 1000) / 1000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.000'
   else begin
      while Length(Result) - Pos('.', Result) < 3 do Result := Result + '0';
   end;
end;

PrintLn('--- SelfieSeg Block Output ---');
for var y := 0 to 3 do begin
   var line := '';
   for var x := 0 to 3 do begin
      var v0 := b_out.GetData([y, x, 0]);
      var v1 := b_out.GetData([y, x, 1]);
      line := line + '(' + RoundedStr(v0) + ', ' + RoundedStr(v1) + ') ';
   end;
   PrintLn(line);
end;