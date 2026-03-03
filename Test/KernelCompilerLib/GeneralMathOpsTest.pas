var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var const_val := k.AddConstant(0.5, [1, 4]);

var v_sub := k.AddSub(in1, in2);
var v_div := k.AddDiv(in1, in2);
var sig := k.AddSigmoid(in1);
var hsig := k.AddHardSigmoid(in1);
var const_add := k.AddAdd(in1, const_val);
var v_exp := k.AddExp(in1);
var v_log := k.AddLog(in1);
var v_pow := k.AddPower(in1, const_val);

k.MarkOutput(v_sub);
k.MarkOutput(v_div);
k.MarkOutput(sig);
k.MarkOutput(hsig);
k.MarkOutput(const_add);
k.MarkOutput(v_exp);
k.MarkOutput(v_log);
k.MarkOutput(v_pow);

var b_in1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_in2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_sub := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_div := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_sig := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_hsig := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_const_add := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_exp := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_log := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);
var b_pow := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4]);

b_in1.SetData([0, 0], 1.0); b_in1.SetData([0, 1], 2.0); b_in1.SetData([0, 2], 0.0); b_in1.SetData([0, 3], -1.0);
b_in2.SetData([0, 0], 0.5); b_in2.SetData([0, 1], 4.0); b_in2.SetData([0, 2], 2.0); b_in2.SetData([0, 3], -1.0);

TKCLKernelCompiler.Dispatch(k, [b_in1, b_in2, b_sub, b_div, b_sig, b_hsig, b_const_add, b_exp, b_log, b_pow]);

PrintLn('Sub: ' + FloatToStr(b_sub.GetData([0, 0])) + ', ' + FloatToStr(b_sub.GetData([0, 1])));
PrintLn('Div: ' + FloatToStr(b_div.GetData([0, 0])) + ', ' + FloatToStr(b_div.GetData([0, 1])));
PrintLn('Sigmoid(0): ' + FloatToStr(b_sig.GetData([0, 2])));
PrintLn('HardSigmoid(-1): ' + FloatToStr(b_hsig.GetData([0, 3])));
PrintLn('ConstAdd(1+0.5): ' + FloatToStr(b_const_add.GetData([0, 0])));
PrintLn('Exp(1): ' + FloatToStr(b_exp.GetData([0, 0])));
PrintLn('Log(2): ' + FloatToStr(b_log.GetData([0, 1])));
PrintLn('Power(2, 0.5): ' + FloatToStr(b_pow.GetData([0, 1])));
