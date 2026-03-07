{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Eric Grange.        }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsKernelCompilerCommon;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
   dwsUtils, dwsXPlatform;

type
   TKCLDataType = (dtInt8, dtFloat16, dtFloat32);
   TKCLDimensions = TArray<Integer>;

   THalfFloat = Word;
   PHalfFloat = ^THalfFloat;

function HalfToFloat(h : THalfFloat) : Single;
function FloatToHalf(f : Single) : THalfFloat;

type
   TKCLStridedBufferDescriptor = record
      DataType : TKCLDataType;
      Dimensions : TKCLDimensions;
      Strides : TArray<NativeInt>;
      BasePointer : Pointer;
      Capacity : NativeInt;
   end;

   TKCLNode = class;
   TKCLNodes = TArray<TKCLNode>;

   TKCLNodeCategory = (ncInput, ncConstant, ncMap, ncStencil, ncReduce, ncOpaque);

   TKCLNode = class
   private
      FName : string;
      FInputs : TKCLNodes;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes); virtual;
      function Category : TKCLNodeCategory; virtual; abstract;
      function Eval(const AInputs : TDoubleDynArray) : Double; virtual;
      property Name : string read FName;
      property Inputs : TKCLNodes read FInputs;
   end;

   TKCLInputNode = class(TKCLNode)
   private
      FInputIndex : Integer;
   public
      constructor Create(const AName : string; AInputIndex : Integer); reintroduce;
      function Category : TKCLNodeCategory; override;
      property InputIndex : Integer read FInputIndex;
   end;

   TKCLConstantNode = class(TKCLNode)
   private
      FValue : Double;
      FDimensions : TKCLDimensions;
   public
      constructor Create(const AName : string; AValue : Double; const ADimensions : TKCLDimensions); reintroduce;
      function Category : TKCLNodeCategory; override;
      property Value : Double read FValue;
      property Dimensions : TKCLDimensions read FDimensions;
   end;

   TKCLMapNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
   end;

   TKCLAddNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLMulNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLSubNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLDequantizeNode = class(TKCLMapNode)
   private
      FScale : Double;
      FZeroPoint : Double;
   public
      constructor Create(const AInputs : TKCLNodes; AScale, AZeroPoint : Double); reintroduce;
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
      property Scale : Double read FScale;
      property ZeroPoint : Double read FZeroPoint;
   end;

   TKCLDivNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLSigmoidNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLHardSigmoidNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLReLU6Node = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLHardSwishNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLExpNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLLogNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLPowerNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLReLUNode = class(TKCLMapNode)
   public
      function Eval(const AInputs : TDoubleDynArray) : Double; override;
   end;

   TKCLStencilNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
   end;

   TKCLReduceNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
   end;

   TKCLConv2DNode = class(TKCLStencilNode)
   private
      FWeights : TDoubleDynArray;
      FBias : TDoubleDynArray;
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer); reintroduce;
      property Weights : TDoubleDynArray read FWeights;
      property Bias : TDoubleDynArray read FBias;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLDepthwiseConv2DNode = class(TKCLStencilNode)
   private
      FWeights : TDoubleDynArray;
      FBias : TDoubleDynArray;
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer); reintroduce;
      property Weights : TDoubleDynArray read FWeights;
      property Bias : TDoubleDynArray read FBias;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLResizeBilinearNode = class(TKCLStencilNode)
   private
      FTargetHeight, FTargetWidth : Integer;
      FAlignCorners, FHalfPixelCenters : Boolean;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; ATargetHeight, ATargetWidth : Integer; AAlignCorners, AHalfPixelCenters : Boolean); reintroduce;
      property TargetHeight : Integer read FTargetHeight;
      property TargetWidth : Integer read FTargetWidth;
      property AlignCorners : Boolean read FAlignCorners;
      property HalfPixelCenters : Boolean read FHalfPixelCenters;
   end;

   TKCLMaxPool2DNode = class(TKCLStencilNode)
   private
      FKernelSize, FStride : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; AKernelSize, AStride : Integer); reintroduce;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLGlobalAvgPoolNode = class(TKCLReduceNode)
   public
   end;

   TKCLSoftMaxNode = class(TKCLReduceNode)
   private
      FAxis : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; AAxis : Integer); reintroduce;
      property Axis : Integer read FAxis;
   end;

   TKCLConcatNode = class(TKCLMapNode)
   private
      FAxis : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; AAxis : Integer); reintroduce;
      property Axis : Integer read FAxis;
   end;

   TKCLConv2DTransposeNode = class(TKCLStencilNode)
   private
      FWeights : TDoubleDynArray;
      FBias : TDoubleDynArray;
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer); reintroduce;
      property Weights : TDoubleDynArray read FWeights;
      property Bias : TDoubleDynArray read FBias;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLKernel = class
   private
      FInputs : TKCLNodes;
      FOutputs : TKCLNodes;
      FNodes : TList<TKCLNode>;
      FOptimizationData : TObject;
   public
      constructor Create;
      destructor Destroy; override;
      function AddInput(const AName : string) : TKCLInputNode;
      procedure AddNode(ANode : TKCLNode);
      procedure MarkOutput(ANode : TKCLNode);
      property Inputs : TKCLNodes read FInputs;
      property Outputs : TKCLNodes read FOutputs;
      property Nodes : TList<TKCLNode> read FNodes;
      property OptimizationData : TObject read FOptimizationData write FOptimizationData;
   end;

   EdwsKCLException = class(Exception)
   public
      class procedure RaiseIndicesCountMismatch(expected, got : Integer);
      class procedure RaiseSpatialDomainLengthMismatch(expected, got : Integer);
      class procedure RaiseSpatialDomainSizeMismatch(dim, expected, got : Integer);
      class procedure RaiseBroadcastingMismatch(dim, d1, d2 : Integer);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// HalfToFloat
//
function HalfToFloat(h : THalfFloat) : Single;
var
   s, e, m : Cardinal;
   res : Cardinal;
begin
   s := (h shr 15) and $00000001;
   e := (h shr 10) and $0000001F;
   m := h and $000003FF;

   if e = 0 then begin
      if m = 0 then begin
         // Plus or minus zero
         res := s shl 31;
      end else begin
         // Denormalized number -- renormalize it
         while (m and $00000400) = 0 do begin
            m := m shl 1;
            Dec(e);
         end;
         Inc(e);
         m := m and $000003FF;
         res := (s shl 31) or ((e + 127 - 15) shl 23) or (m shl 13);
      end;
   end else if e = 31 then begin
      // Positive or negative infinity or NaN
      res := (s shl 31) or ($FF shl 23) or (m shl 13);
   end else begin
      // Normalized number
      res := (s shl 31) or ((e + 127 - 15) shl 23) or (m shl 13);
   end;
   Result := PSingle(@res)^;
end;

// FloatToHalf
//
function FloatToHalf(f : Single) : THalfFloat;
var
   i : Cardinal;
   s, e, m : Cardinal;
begin
   i := PCardinal(@f)^;
   s := (i shr 16) and $00008000;
   e := ((i shr 23) and $000000FF);
   m := i and $007FFFFF;

   if e = 0 then begin
      // Plus or minus zero
      Result := s;
   end else if e = $FF then begin
      if m = 0 then begin
         // Plus or minus infinity
         Result := s or $7C00;
      end else begin
         // NaN
         Result := s or $7E00;
      end;
   end else begin
      if e < (127 - 15) then begin
         // Underflow to zero (or denormal, but let's stick to zero for reference)
         Result := s;
      end else if e > (127 + 15) then begin
         // Overflow to infinity
         Result := s or $7C00;
      end else begin
         // Normalized number
         Result := s or ((e - 127 + 15) shl 10) or (m shr 13);
      end;
   end;
end;

// ------------------
// ------------------ TKCLNode ------------------
// ------------------

// Create
//
constructor TKCLNode.Create(const AName : string; const AInputs : TKCLNodes);
begin
   FName := AName; FInputs := AInputs;
end;

// Eval
//
function TKCLNode.Eval(const AInputs : TDoubleDynArray) : Double;
begin
   Result := 0;
end;

// ------------------
// ------------------ TKCLInputNode ------------------
// ------------------

// Create
//
constructor TKCLInputNode.Create(const AName : string; AInputIndex : Integer);
begin
   inherited Create(AName, []); FInputIndex := AInputIndex;
end;

// Category
//
function TKCLInputNode.Category : TKCLNodeCategory; begin Result := ncInput; end;

// ------------------
// ------------------ TKCLConstantNode ------------------
// ------------------

// Create
//
constructor TKCLConstantNode.Create(const AName : string; AValue : Double; const ADimensions : TKCLDimensions);
begin
   inherited Create(AName, []); FValue := AValue; FDimensions := ADimensions;
end;

// Category
//
function TKCLConstantNode.Category : TKCLNodeCategory; begin Result := ncConstant; end;

// ------------------
// ------------------ TKCLMapNode ------------------
// ------------------

// Category
//
function TKCLMapNode.Category : TKCLNodeCategory; begin Result := ncMap; end;

// ------------------
// ------------------ TKCLAddNode ------------------
// ------------------

// Eval
//
function TKCLAddNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := AInputs[0] + AInputs[1]; end;

// ------------------
// ------------------ TKCLMulNode ------------------
// ------------------

// Eval
//
function TKCLMulNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := AInputs[0] * AInputs[1]; end;

// ------------------
// ------------------ TKCLSubNode ------------------
// ------------------

// Eval
//
function TKCLSubNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := AInputs[0] - AInputs[1]; end;

// ------------------
// ------------------ TKCLDequantizeNode ------------------
// ------------------

constructor TKCLDequantizeNode.Create(const AInputs : TKCLNodes; AScale, AZeroPoint : Double);
begin
   inherited Create('dequantize', AInputs);
   FScale := AScale;
   FZeroPoint := AZeroPoint;
end;

// Eval
//
function TKCLDequantizeNode.Eval(const AInputs : TDoubleDynArray) : Double;
begin
   Result := (AInputs[0] - FZeroPoint) * FScale;
end;

// ------------------
// ------------------ TKCLDivNode ------------------
// ------------------

// Eval
//
function TKCLDivNode.Eval(const AInputs : TDoubleDynArray) : Double; begin if AInputs[1] <> 0 then Result := AInputs[0] / AInputs[1] else Result := 0; end;

// ------------------
// ------------------ TKCLSigmoidNode ------------------
// ------------------

// Eval
//
function TKCLSigmoidNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := 1 / (1 + Exp(-AInputs[0])); end;

// ------------------
// ------------------ TKCLHardSigmoidNode ------------------
// ------------------

// Eval
//
function TKCLHardSigmoidNode.Eval(const AInputs : TDoubleDynArray) : Double;
begin
   Result := (AInputs[0] + 3.0) / 6.0;
   if Result < 0 then Result := 0 else if Result > 1 then Result := 1;
end;

// ------------------
// ------------------ TKCLReLU6Node ------------------
// ------------------

// Eval
//
function TKCLReLU6Node.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := Max(0, Min(6, AInputs[0])); end;

// ------------------
// ------------------ TKCLHardSwishNode ------------------
// ------------------

// Eval
//
function TKCLHardSwishNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := AInputs[0] * Max(0, Min(1, AInputs[0] * 0.16666667 + 0.5)); end;

// ------------------
// ------------------ TKCLExpNode ------------------
// ------------------

// Eval
//
function TKCLExpNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := Exp(AInputs[0]); end;

// ------------------
// ------------------ TKCLLogNode ------------------
// ------------------

// Eval
//
function TKCLLogNode.Eval(const AInputs : TDoubleDynArray) : Double; begin if AInputs[0] > 0 then Result := LogN(2.718281828459, AInputs[0]) else Result := -1e30; end;

// ------------------
// ------------------ TKCLPowerNode ------------------
// ------------------

// Eval
//
function TKCLPowerNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := Power(AInputs[0], AInputs[1]); end;

// ------------------
// ------------------ TKCLReLUNode ------------------
// ------------------

// Eval
//
function TKCLReLUNode.Eval(const AInputs : TDoubleDynArray) : Double; begin Result := Max(0, AInputs[0]); end;

// ------------------
// ------------------ TKCLStencilNode ------------------
// ------------------

// Category
//
function TKCLStencilNode.Category : TKCLNodeCategory; begin Result := ncStencil; end;

// ------------------
// ------------------ TKCLReduceNode ------------------
// ------------------

// Category
//
function TKCLReduceNode.Category : TKCLNodeCategory; begin Result := ncReduce; end;

// ------------------
// ------------------ TKCLConv2DNode ------------------
// ------------------

// Create
//
constructor TKCLConv2DNode.Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer);
begin
   inherited Create(AName, AInputs); FWeights := AWeights; FBias := ABias; FKernelSize := AKernelSize; FStride := AStride;
end;

// ------------------
// ------------------ TKCLDepthwiseConv2DNode ------------------
// ------------------

// Create
//
constructor TKCLDepthwiseConv2DNode.Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer);
begin
   inherited Create(AName, AInputs); FWeights := AWeights; FBias := ABias; FKernelSize := AKernelSize; FStride := AStride;
end;

// ------------------
// ------------------ TKCLResizeBilinearNode ------------------
// ------------------

// Create
//
constructor TKCLResizeBilinearNode.Create(const AName : string; const AInputs : TKCLNodes; ATargetHeight, ATargetWidth : Integer; AAlignCorners, AHalfPixelCenters : Boolean);
begin
   inherited Create(AName, AInputs); FTargetHeight := ATargetHeight; FTargetWidth := ATargetWidth; FAlignCorners := AAlignCorners; FHalfPixelCenters := AHalfPixelCenters;
end;

// ------------------
// ------------------ TKCLMaxPool2DNode ------------------
// ------------------

// Create
//
constructor TKCLMaxPool2DNode.Create(const AName : string; const AInputs : TKCLNodes; AKernelSize, AStride : Integer);
begin
   inherited Create(AName, AInputs); FKernelSize := AKernelSize; FStride := AStride;
end;

// ------------------
// ------------------ TKCLSoftMaxNode ------------------
// ------------------

// Create
//
constructor TKCLSoftMaxNode.Create(const AName : string; const AInputs : TKCLNodes; AAxis : Integer);
begin
   inherited Create(AName, AInputs); FAxis := AAxis;
end;

// ------------------
// ------------------ TKCLConcatNode ------------------
// ------------------

// Create
//
constructor TKCLConcatNode.Create(const AName : string; const AInputs : TKCLNodes; AAxis : Integer);
begin
   inherited Create(AName, AInputs); FAxis := AAxis;
end;

// ------------------
// ------------------ TKCLConv2DTransposeNode ------------------
// ------------------

// Create
//
constructor TKCLConv2DTransposeNode.Create(const AName : string; const AInputs : TKCLNodes; const AWeights, ABias : TDoubleDynArray; AKernelSize, AStride : Integer);
begin
   inherited Create(AName, AInputs); FWeights := AWeights; FBias := ABias; FKernelSize := AKernelSize; FStride := AStride;
end;

// ------------------
// ------------------ TKCLKernel ------------------
// ------------------

// Create
//
constructor TKCLKernel.Create; begin FNodes := TList<TKCLNode>.Create; end;

// Destroy
//
destructor TKCLKernel.Destroy; begin for var node in FNodes do node.Free; FNodes.Free; FOptimizationData.Free; inherited; end;

// AddInput
//
function TKCLKernel.AddInput(const AName : string) : TKCLInputNode;
begin
   Result := TKCLInputNode.Create(AName, Length(FInputs)); SetLength(FInputs, Length(FInputs) + 1); FInputs[High(FInputs)] := Result; FNodes.Add(Result);
end;

// AddNode
//
procedure TKCLKernel.AddNode(ANode : TKCLNode); begin FNodes.Add(ANode); end;

// MarkOutput
//
procedure TKCLKernel.MarkOutput(ANode : TKCLNode); begin SetLength(FOutputs, Length(FOutputs) + 1); FOutputs[High(FOutputs)] := ANode; end;

// ------------------
// ------------------ EdwsKCLException ------------------
// ------------------

// RaiseIndicesCountMismatch
//
class procedure EdwsKCLException.RaiseIndicesCountMismatch(expected, got : Integer);
begin
   raise CreateFmt('KCL: Indices count mismatch (expected %d, passed %d).', [expected, got]);
end;

// RaiseSpatialDomainLengthMismatch
//
class procedure EdwsKCLException.RaiseSpatialDomainLengthMismatch(expected, got : Integer);
begin
   raise CreateFmt('KCL: Output spatial domain length mismatch (node expected %d, buffer has %d).', [expected, got]);
end;

// RaiseSpatialDomainSizeMismatch
//
class procedure EdwsKCLException.RaiseSpatialDomainSizeMismatch(dim, expected, got : Integer);
begin
   raise CreateFmt('KCL: Output spatial domain size mismatch at dimension %d (node expected %d, buffer has %d).', [dim, expected, got]);
end;

// RaiseBroadcastingMismatch
//
class procedure EdwsKCLException.RaiseBroadcastingMismatch(dim, d1, d2 : Integer);
begin
   raise CreateFmt('KCL: Incompatible shapes for broadcasting at dimension %d (got %d and %d).', [dim, d1, d2]);
end;

end.
