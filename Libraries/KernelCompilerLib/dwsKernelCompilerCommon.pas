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
   System.Classes, System.SysUtils, System.Math, dwsUtils;

type
   EdwsKCLException = class (Exception)
   public
      class procedure RaiseSpatialDomainLengthMismatch(AExpected, AActual : Integer);
      class procedure RaiseSpatialDomainSizeMismatch(ADim, AExpected, AActual : Integer);
      class procedure RaiseBroadcastingMismatch(ADim, AVal1, AVal2 : Integer);
      class procedure RaiseIndicesCountMismatch(AExpected, AActual : Integer);
   end;

   TKCLDataType = (dtInt8, dtFloat16, dtFloat32);

   THalfFloat = Word;
   PHalfFloat = ^THalfFloat;

function HalfToFloat(h : THalfFloat) : Single;
function FloatToHalf(f : Single) : THalfFloat;

type
   TKCLDimensions = array of Integer;
   TKCLStrides = array of Int32;

   PKCLStridedBufferDescriptor = ^TKCLStridedBufferDescriptor;
   TKCLStridedBufferDescriptor = record
      BasePointer : Pointer;
      Capacity : NativeInt;
      DataType : TKCLDataType;
      Dimensions : TKCLDimensions;
      Strides : TKCLStrides;
   end;

   TKCLNodeCategory = (ncInput, ncMap, ncStencil, ncReduce);

   TKCLNode = class;
   TKCLNodes = array of TKCLNode;

   TKCLNode = class
   private
      FInputs : TKCLNodes;
      FName : String;
      FOutputIndex : Integer; // -1 if not a kernel output
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes); virtual;
      function Category : TKCLNodeCategory; virtual; abstract;
      property Name : String read FName;
      property Inputs : TKCLNodes read FInputs;
      property OutputIndex : Integer read FOutputIndex write FOutputIndex;
   end;

   TKCLInputNode = class(TKCLNode)
   private
      FInputIndex : Integer;
   public
      constructor Create(const AName : String; AInputIndex : Integer); reintroduce;
      function Category : TKCLNodeCategory; override;
      property InputIndex : Integer read FInputIndex;
   end;

   TKCLInputNodes = array of TKCLInputNode;

   TKCLMapNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
      function Eval(const AValues : TDoubleDynArray) : Double; virtual; abstract;
   end;

   TKCLStencilNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
   end;

   TKCLReduceNode = class(TKCLNode)
   public
      function Category : TKCLNodeCategory; override;
   end;

   // Specific nodes
   TKCLAddNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLMulNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLSubNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLDivNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLSigmoidNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLHardSigmoidNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLExpNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLLogNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLPowerNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLConstantNode = class(TKCLInputNode)
   private
      FValue : Double;
      FDimensions : TKCLDimensions;
   public
      constructor Create(const AName : String; AValue : Double; const ADimensions : TKCLDimensions); reintroduce;
      property Value : Double read FValue;
      property Dimensions : TKCLDimensions read FDimensions;
   end;

   TKCLReLUNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLReLU6Node = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLHardSwishNode = class(TKCLMapNode)
   public
      function Eval(const AValues : TDoubleDynArray) : Double; override;
   end;

   TKCLConv2DNode = class(TKCLStencilNode)
   private
      FWeights : TDoubleDynArray;
      FBias : TDoubleDynArray;
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer); reintroduce;
      property Weights : TDoubleDynArray read FWeights;
      property Bias : TDoubleDynArray read FBias;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLConv2DTransposeNode = class(TKCLStencilNode)
   private
      FWeights : TDoubleDynArray;
      FBias : TDoubleDynArray;
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer); reintroduce;
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
      constructor Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer); reintroduce;
      property Weights : TDoubleDynArray read FWeights;
      property Bias : TDoubleDynArray read FBias;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLResizeBilinearNode = class(TKCLStencilNode)
   private
      FTargetHeight : Integer;
      FTargetWidth : Integer;
      FAlignCorners : Boolean;
      FHalfPixelCenters : Boolean;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; ATargetHeight, ATargetWidth : Integer; AAlignCorners: Boolean = False; AHalfPixelCenters: Boolean = False); reintroduce;
      property TargetHeight : Integer read FTargetHeight;
      property TargetWidth : Integer read FTargetWidth;
      property AlignCorners : Boolean read FAlignCorners;
      property HalfPixelCenters : Boolean read FHalfPixelCenters;
   end;

   TKCLConcatNode = class(TKCLMapNode)
   private
      FAxis : Integer;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; AAxis : Integer); reintroduce;
      function Eval(const AValues : TDoubleDynArray) : Double; override;
      property Axis : Integer read FAxis;
   end;

   TKCLMaxPool2DNode = class(TKCLStencilNode)
   private
      FKernelSize : Integer;
      FStride : Integer;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; AKernelSize, AStride: Integer); reintroduce;
      property KernelSize : Integer read FKernelSize;
      property Stride : Integer read FStride;
   end;

   TKCLGlobalAvgPoolNode = class(TKCLReduceNode)
   end;

   TKCLSoftMaxNode = class(TKCLNode)
   private
      FAxis : Integer;
   public
      constructor Create(const AName : String; const AInputs : TKCLNodes; AAxis : Integer); reintroduce;
      function Category : TKCLNodeCategory; override;
      property Axis : Integer read FAxis;
   end;

   TKCLKernel = class
   private
      FNodes : TKCLNodes;
      FInputs : TKCLInputNodes;
      FOutputs : TKCLNodes;
      FGraphVersion : Integer;
      FOptimizationData : TObject;
   public
      destructor Destroy; override;
      function AddInput(const AName : String) : TKCLInputNode;
      procedure AddNode(ANode : TKCLNode);
      procedure MarkOutput(ANode : TKCLNode);
      property Nodes : TKCLNodes read FNodes;
      property Inputs : TKCLInputNodes read FInputs;
      property Outputs : TKCLNodes read FOutputs;
      property GraphVersion : Integer read FGraphVersion;
      property OptimizationData : TObject read FOptimizationData write FOptimizationData;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{ EdwsKCLException }

class procedure EdwsKCLException.RaiseSpatialDomainLengthMismatch(AExpected, AActual : Integer);
begin
   raise EdwsKCLException.CreateFmt('KCL: Output spatial domain length mismatch (node expected %d, buffer has %d).', [AExpected, AActual]);
end;

class procedure EdwsKCLException.RaiseSpatialDomainSizeMismatch(ADim, AExpected, AActual : Integer);
begin
   raise EdwsKCLException.CreateFmt('KCL: Output spatial domain size mismatch at dimension %d (node expected %d, buffer has %d).', [ADim, AExpected, AActual]);
end;

class procedure EdwsKCLException.RaiseBroadcastingMismatch(ADim, AVal1, AVal2 : Integer);
begin
   raise EdwsKCLException.CreateFmt('KCL: Incompatible shapes for broadcasting at dimension %d (got %d and %d).', [ADim, AVal1, AVal2]);
end;

class procedure EdwsKCLException.RaiseIndicesCountMismatch(AExpected, AActual : Integer);
begin
   raise EdwsKCLException.CreateFmt('KCL: Indices count mismatch (expected %d, passed %d).', [AExpected, AActual]);
end;

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
constructor TKCLNode.Create(const AName : String; const AInputs : TKCLNodes);
begin
   FName := AName;
   FInputs := AInputs;
   FOutputIndex := -1;
end;

// ------------------
// ------------------ TKCLInputNode ------------------
// ------------------

// Create
//
constructor TKCLInputNode.Create(const AName : String; AInputIndex : Integer);
begin
   inherited Create(AName, nil);
   FInputIndex := AInputIndex;
end;

// Category
//
function TKCLInputNode.Category : TKCLNodeCategory;
begin
   Result := ncInput;
end;

// ------------------
// ------------------ TKCLMapNode ------------------
// ------------------

// Category
//
function TKCLMapNode.Category : TKCLNodeCategory;
begin
   Result := ncMap;
end;

// ------------------
// ------------------ TKCLStencilNode ------------------
// ------------------

// Category
//
function TKCLStencilNode.Category : TKCLNodeCategory;
begin
   Result := ncStencil;
end;

// ------------------
// ------------------ TKCLReduceNode ------------------
// ------------------

// Category
//
function TKCLReduceNode.Category : TKCLNodeCategory;
begin
   Result := ncReduce;
end;

// ------------------
// ------------------ TKCLAddNode ------------------
// ------------------

// Eval
//
function TKCLAddNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := AValues[0] + AValues[1];
end;

// ------------------
// ------------------ TKCLMulNode ------------------
// ------------------

// Eval
//
function TKCLMulNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := AValues[0] * AValues[1];
end;

// ------------------
// ------------------ TKCLSubNode ------------------
// ------------------

// Eval
//
function TKCLSubNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := AValues[0] - AValues[1];
end;

// ------------------
// ------------------ TKCLDivNode ------------------
// ------------------

// Eval
//
function TKCLDivNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   if AValues[1] <> 0.0 then Result := AValues[0] / AValues[1] else Result := 0.0;
end;

// ------------------
// ------------------ TKCLSigmoidNode ------------------
// ------------------

// Eval
//
function TKCLSigmoidNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := 1.0 / (1.0 + Exp(-AValues[0]));
end;

// ------------------
// ------------------ TKCLHardSigmoidNode ------------------
// ------------------

// Eval
//
function TKCLHardSigmoidNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := (AValues[0] + 3.0) / 6.0;
   if Result < 0.0 then Result := 0.0
   else if Result > 1.0 then Result := 1.0;
end;

// ------------------
// ------------------ TKCLExpNode ------------------
// ------------------

// Eval
//
function TKCLExpNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := Exp(AValues[0]);
end;

// ------------------
// ------------------ TKCLLogNode ------------------
// ------------------

// Eval
//
function TKCLLogNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   if AValues[0] > 0.0 then Result := Ln(AValues[0]) else Result := 0.0;
end;

// ------------------
// ------------------ TKCLPowerNode ------------------
// ------------------

// Eval
//
function TKCLPowerNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := Power(AValues[0], AValues[1]);
end;

// ------------------
// ------------------ TKCLConstantNode ------------------
// ------------------

// Create
//
constructor TKCLConstantNode.Create(const AName : String; AValue : Double; const ADimensions : TKCLDimensions);
begin
   inherited Create(AName, -1); // -1 as it's not a standard input
   FValue := AValue;
   FDimensions := ADimensions;
end;

// ------------------
// ------------------ TKCLReLUNode ------------------
// ------------------

// Eval
//
function TKCLReLUNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   if AValues[0] > 0.0 then Result := AValues[0] else Result := 0.0;
end;

// ------------------
// ------------------ TKCLReLU6Node ------------------
// ------------------

// Eval
//
function TKCLReLU6Node.Eval(const AValues : TDoubleDynArray) : Double;
begin
   if AValues[0] > 6.0 then Result := 6.0
   else if AValues[0] > 0.0 then Result := AValues[0]
   else Result := 0.0;
end;

// ------------------
// ------------------ TKCLHardSwishNode ------------------
// ------------------

// Eval
//
function TKCLHardSwishNode.Eval(const AValues : TDoubleDynArray) : Double;
var
   v : Double;
begin
   v := AValues[0] + 3.0;
   if v < 0.0 then v := 0.0
   else if v > 6.0 then v := 6.0;
   Result := AValues[0] * v / 6.0;
end;

// ------------------
// ------------------ TKCLConv2DNode ------------------
// ------------------

constructor TKCLConv2DNode.Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer);
begin
   inherited Create(AName, AInputs);
   FWeights := AWeights;
   FBias := ABias;
   FKernelSize := AKernelSize;
   FStride := AStride;
end;

// ------------------
// ------------------ TKCLConv2DTransposeNode ------------------
// ------------------

constructor TKCLConv2DTransposeNode.Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer);
begin
   inherited Create(AName, AInputs);
   FWeights := AWeights;
   FBias := ABias;
   FKernelSize := AKernelSize;
   FStride := AStride;
end;

// ------------------
// ------------------ TKCLDepthwiseConv2DNode ------------------
// ------------------

constructor TKCLDepthwiseConv2DNode.Create(const AName : String; const AInputs : TKCLNodes; const AWeights, ABias: TDoubleDynArray; AKernelSize, AStride: Integer);
begin
   inherited Create(AName, AInputs);
   FWeights := AWeights;
   FBias := ABias;
   FKernelSize := AKernelSize;
   FStride := AStride;
end;

// ------------------
// ------------------ TKCLResizeBilinearNode ------------------
// ------------------

constructor TKCLResizeBilinearNode.Create(const AName : String; const AInputs : TKCLNodes; ATargetHeight, ATargetWidth : Integer; AAlignCorners: Boolean = False; AHalfPixelCenters: Boolean = False);
begin
   inherited Create(AName, AInputs);
   FTargetHeight := ATargetHeight;
   FTargetWidth := ATargetWidth;
   FAlignCorners := AAlignCorners;
   FHalfPixelCenters := AHalfPixelCenters;
end;

// ------------------
// ------------------ TKCLConcatNode ------------------
// ------------------

constructor TKCLConcatNode.Create(const AName : String; const AInputs : TKCLNodes; AAxis : Integer);
begin
   inherited Create(AName, AInputs);
   FAxis := AAxis;
end;

function TKCLConcatNode.Eval(const AValues : TDoubleDynArray) : Double;
begin
   Result := 0; // Not used normally, Concat needs special backend handling
end;

// ------------------
// ------------------ TKCLMaxPool2DNode ------------------
// ------------------

constructor TKCLMaxPool2DNode.Create(const AName : String; const AInputs : TKCLNodes; AKernelSize, AStride: Integer);
begin
   inherited Create(AName, AInputs);
   FKernelSize := AKernelSize;
   FStride := AStride;
end;

// ------------------
// ------------------ TKCLSoftMaxNode ------------------
// ------------------

constructor TKCLSoftMaxNode.Create(const AName : String; const AInputs : TKCLNodes; AAxis : Integer);
begin
   inherited Create(AName, AInputs);
   FAxis := AAxis;
end;

function TKCLSoftMaxNode.Category : TKCLNodeCategory;
begin
   Result := ncReduce;
end;

// ------------------
// ------------------ TKCLKernel ------------------
// ------------------

// Destroy
//
destructor TKCLKernel.Destroy;
begin
   for var i := 0 to High(FNodes) do
      FNodes[i].Free;
   FOptimizationData.Free;
   inherited;
end;

// AddInput
//
function TKCLKernel.AddInput(const AName : String) : TKCLInputNode;
begin
   Result := TKCLInputNode.Create(AName, Length(FInputs));
   SetLength(FInputs, Length(FInputs) + 1);
   FInputs[High(FInputs)] := Result;
   AddNode(Result);
end;

// AddNode
//
procedure TKCLKernel.AddNode(ANode : TKCLNode);
begin
   SetLength(FNodes, Length(FNodes) + 1);
   FNodes[High(FNodes)] := ANode;
   Inc(FGraphVersion);
end;

// MarkOutput
//
procedure TKCLKernel.MarkOutput(ANode : TKCLNode);
begin
   ANode.OutputIndex := Length(FOutputs);
   SetLength(FOutputs, Length(FOutputs) + 1);
   FOutputs[High(FOutputs)] := ANode;
   Inc(FGraphVersion);
end;

end.
