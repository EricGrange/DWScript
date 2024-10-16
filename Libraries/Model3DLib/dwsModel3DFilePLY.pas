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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsModel3DFilePLY;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils, System.Types,
   dwsStringIterator, dwsUtils;

type
   TPLYFormat = (
      ply_ascii,
      ply_binary_little_endian,
      ply_binary_big_endian
   );

   TPLYPropertyType = (
      plyt_none = 0,
      plyt_char,      // character                 1
      plyt_uchar,     // unsigned character        1
      plyt_short,     // short integer             2
      plyt_ushort,    // unsigned short integer    2
      plyt_int,       // integer                   4
      plyt_uint,      // unsigned integer          4
      plyt_float,     // single-precision float    4
      plyt_double     // double-precision float    8
   );

const
   cPLYPropertyByteLength : array [ TPLYPropertyType ] of Byte = (
      0, 1, 1, 2, 2, 4, 4, 4, 8
   );
   cPLYPropertyString : array [ TPLYPropertyType ] of String = (
      '', 'char', 'uchar', 'short', 'ushort', 'int', 'uint', 'float', 'double'
   );

type
   TPLYEndianSwapContext = record
      Ptr : PByte;
      BytesRemaining : Integer;
      CurrentIsNativeEndian : Boolean;

      // swap current value and advance pointer
      procedure EndianSwapType(typ : TPLYPropertyType);
   end;

   TPLYProperty = class
      private
         FName : String;
         FListTyp : TPLYPropertyType;
         FTyp : TPLYPropertyType;

         procedure EndianSwapData(var context : TPLYEndianSwapContext);

      public
         property Name : String read FName;
         property ListTyp : TPLYPropertyType read FListTyp;
         property Typ : TPLYPropertyType read FTyp;
   end;

   TPLYProperties = array of TPLYProperty;

   TPLYElement = class
      private
         FName : String;
         FCount : Integer;
         FProperties : TPLYProperties;
         FData : TBytes;

         function GetProperties(index : Integer) : TPLYProperty;

      public
         destructor Destroy; override;

         property Name : String read FName write FName;
         property Count : Integer read FCount write FCount;

         property Data : TBytes read FData write FData;
         procedure EndianSwapData(currentIsNativeEndian : Boolean);

         property Properties[index : Integer] : TPLYProperty read GetProperties;
         function PropertyCount : Integer;

         function AddProperty(const aName : String; aListTyp, aTyp : TPLYPropertyType) : TPLYProperty;

         function IndexOfProperty(const name : String) : Integer;
         function PropertyNames : TStringDynArray;
         function HasListProperties : Boolean;

         procedure ClearProperties;
   end;

   TPLYElements = array of TPLYElement;

   TModel3DFilePLY = class
      private
         FFormat : TPLYFormat;
         FElements : TPLYElements;

      protected
         function GetElements(index : Integer) : TPLYElement;

      public
         constructor Create;
         destructor Destroy; override;

         function LoadFromData(const data : RawByteString; errors : TStrings) : Boolean;

         property Elements[index : Integer] : TPLYElement read GetElements;
         function ElementCount : Integer;

         procedure ClearElements;

   end;

   EPLYException = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsXPlatform;

type
   TPLYParserContext = record
      private
         FPLY : TModel3DFilePLY;     // referred, not owned
         FIter : TStringIterator;
         FErrors : TStrings;        // referred, not owned

         function ReportError(const msg : String) : Boolean;
         function ReportErrorFmt(const msg : String; const args : array of const) : Boolean;

         function ParseFormat : Boolean;
         function ParseElement : Boolean;

      public
         property Errors : TStrings read FErrors write FErrors;

         // parse ASCII header, return True in case of success, False otherwise and document errors
         function ParseHeader(const plyHeader : String; aPly : TModel3DFilePLY) : Boolean;
         function ParseData(const plyData : String; aPly : TModel3DFilePLY) : Boolean;
   end;

// PropertyTypeByString
//
function PropertyTypeByString(const aStr : String) : TPLYPropertyType;
begin
   Result := plyt_none;
   for var i := Succ(plyt_none) to High(TPLYPropertyType) do begin
      if aStr = cPLYPropertyString[i] then begin
         Result := i;
         Break;
      end;
   end;
end;

// EndianSwapType
//
procedure TPLYEndianSwapContext.EndianSwapType(typ : TPLYPropertyType);
begin
   var n := cPLYPropertyByteLength[typ];
   Assert(BytesRemaining >= n);
   case n of
      2 : SwapBytesInt16(Pointer(Ptr), Pointer(Ptr));
      4 : SwapBytesInt32(PInt32(Ptr), PInt32(Ptr));
      8 : SwapBytesInt64(PInt64(Ptr), PInt64(Ptr));
   end;
   Inc(Ptr, n);
   Dec(BytesRemaining, n);
end;

// GetNativeIntegerFromType
//
function GetNativeIntegerFromType(p : Pointer; typ : TPLYPropertyType) : Int64;
begin
   case typ of
      plyt_char : Result := PShortInt(p)^;
      plyt_uchar : Result := PByte(p)^;
      plyt_short : Result := PSmallInt(p)^;
      plyt_ushort : Result := PWord(p)^;
      plyt_int : Result := PInteger(p)^;
      plyt_uint : Result := PUInt32(p)^;
      plyt_float : Result := Round(PSingle(p)^);
      plyt_double : Result := Round(PDouble(p)^);
   else
      Result := 0;
      Assert(False);
   end;
end;

// GetSwappedIntegerFromType
//
function GetSwappedIntegerFromType(p : Pointer; typ : TPLYPropertyType) : Int64;
begin
   case typ of
      plyt_char : Result := PShortInt(p)^;
      plyt_uchar : Result := PByte(p)^;
      plyt_short : begin
         var buf : Word;
         SwapBytesInt16(p, @buf);
         Result := SmallInt(buf);
      end;
      plyt_ushort : begin
         var buf : Word;
         SwapBytesInt16(p, @buf);
         Result := buf;
      end;
      plyt_int : begin
         var buf : Integer;
         SwapBytesInt32(p, @buf);
         Result := buf;
      end;
      plyt_uint : begin
         var buf : UInt32;
         SwapBytesInt32(p, @buf);
         Result := buf;
      end;
      plyt_float : begin
         var buf : Single;
         SwapBytesInt32(p, @buf);
         Result := Round(buf);
      end;
      plyt_double : begin
         var buf : Double;
         SwapBytesInt64(p, @buf);
         Result := Round(buf);
      end;
   else
      Result := 0;
      Assert(False);
   end;
end;

// ------------------
// ------------------ TPLYParserContext ------------------
// ------------------

// ReportError
//
function TPLYParserContext.ReportError(const msg : String) : Boolean;
begin
   if Errors <> nil then
      Errors.Add(msg);
   Result := False;
end;

// ReportErrorFmt
//
function TPLYParserContext.ReportErrorFmt(const msg : String; const args : array of const) : Boolean;
begin
   Result := ReportError(Format(msg, args));
end;

// ParseHeader
//
function TPLYParserContext.ParseHeader(const plyHeader : String; aPly : TModel3DFilePLY) : Boolean;
var
   attribute : String;
begin
   FIter := TStringIterator.Create(plyHeader);
   FPLY := aPly;
   try
      if FIter.CollectAlphaNumeric <> 'ply' then
         Exit(ReportError('"ply" magic header expected'));
      FIter.SkipWhiteSpace;

      if not ParseFormat then Exit(False);

      while True do begin
         attribute := FIter.CollectAlphaNumericUnderscore;
         if (attribute = 'comment') or (attribute = 'obj_info') then begin
            // ignore comments for now
            FIter.SkipUntilEOL;
            FIter.SkipWhiteSpace;
         end else if attribute = 'element' then begin
            if not ParseElement then Exit(False);
         end else if attribute = 'end_header' then
            Break
         else if attribute = '' then
            Exit(ReportError('Unexpected end of header'))
         else Exit(ReportErrorFmt('Unsupported attribute "%s"', [ attribute ]));
      end;

      Result := True;
   finally
      FreeAndNil(FIter);
      FPLY := nil;
   end;
end;

// ParseData
//
function TPLYParserContext.ParseData(const plyData : String; aPly : TModel3DFilePLY) : Boolean;
begin
   FIter := TStringIterator.Create(plyData);
   FPLY := aPly;
   try
      for var element in aPly.FElements do begin
         var wobs := TWriteOnlyBlockStream.AllocFromPool;
         try
            for var i := 1 to element.Count do begin
               for var prop in element.FProperties do begin
                  case prop.Typ of
                     plyt_char : begin
                        var v := FIter.CollectInteger;
                        if ShortInt(v) <> v then ReportErrorFmt('char out of range (%d)', [ v ]);
                        wobs.WriteShortInt(ShortInt(v));
                     end;
                     plyt_uchar : begin
                        var v := FIter.CollectInteger;
                        if Byte(v) <> v then ReportErrorFmt('uchar out of range (%d)', [ v ]);
                        wobs.WriteByte(Byte(v));
                     end;
                     plyt_short : begin
                        var v := FIter.CollectInteger;
                        if SmallInt(v) <> v then ReportErrorFmt('short out of range (%d)', [ v ]);
                        wobs.WriteSmallInt(SmallInt(v));
                     end;
                     plyt_ushort : begin
                        var v := FIter.CollectInteger;
                        if Word(v) <> v then ReportErrorFmt('ushort out of range (%d)', [ v ]);
                        wobs.WriteWord(Word(v));
                     end;
                     plyt_int : begin
                        var v := FIter.CollectInteger;
                        if Int32(v) <> v then ReportErrorFmt('int out of range (%d)', [ v ]);
                        wobs.WriteWord(Integer(v));
                     end;
                     plyt_uint : begin
                        var v := FIter.CollectInteger;
                        if UInt32(v) <> v then ReportErrorFmt('uint out of range (%d)', [ v ]);
                        wobs.WriteDWord(UInt32(v));
                     end;
                     plyt_float : begin
                        var v : Double := 0;
                        if not FIter.CollectFloat(v) then
                           ReportError('float failed to parse');
                        wobs.WriteSingle(v);
                     end;
                     plyt_double : begin
                        var v : Double := 0;
                        if not FIter.CollectFloat(v) then
                           ReportError('double failed to parse');
                        wobs.WriteDouble(v);
                     end;
                  else
                     Exit(ReportError('Unusupported type in ParseData'));
                  end;
                  FIter.SkipWhiteSpace;
               end;
            end;
            element.FData := wobs.ToBytes;
         finally
            wobs.ReturnToPool;
         end;
      end;
      Result := (FErrors.Count = 0);
   finally
      FreeAndNil(FIter);
      FPLY := nil;
   end;
end;

// ParseFormat
//
function TPLYParserContext.ParseFormat : Boolean;
var
   fmt : String;
   version : Double;
begin
   if FIter.CollectAlphaNumeric <> 'format' then
      Exit(ReportError('"format" expected'));
   FIter.SkipWhiteSpace;

   fmt := FIter.CollectAlphaNumericUnderscore;
   if fmt = 'ascii' then
      FPLY.FFormat := ply_ascii
   else if fmt = 'binary_little_endian' then
      FPLY.FFormat := ply_binary_little_endian
   else if fmt = 'binary_big_endian' then
      FPLY.FFormat := ply_binary_big_endian
   else Exit(ReportErrorFmt('Unsupported format "%s"', [ fmt ]));
   FIter.SkipWhiteSpace;

   if (not FIter.CollectFloat(version)) or (version <> 1) then
      Exit(ReportError('Unsupported format version'));
   FIter.SkipWhiteSpace;

   Result := True;
end;

// ParseElement
//
function TPLYParserContext.ParseElement : Boolean;
var
   elementName : String;
   propertyName : String;
   propertyTypeString : String;
   propertyType, listType : TPLYPropertyType;
begin
   FIter.SkipWhiteSpace;
   elementName := FIter.CollectAlphaNumericUnderscore;
   if elementName = '' then
      Exit(ReportError('Missing element name'));
   FIter.SkipWhiteSpace;

   var elementCount := FIter.CollectInteger;
   if elementCount <= 0 then
      Exit(ReportError('Invalid element count'));
   FIter.SkipWhiteSpace;

   var element := TPLYElement.Create;
   element.FName := elementName;
   element.FCount := elementCount;
   FPLY.FElements := FPLY.FElements + [ element ];

   while FIter.Peek('property') do begin
      if FIter.CollectAlphaNumeric <> 'property' then
         Exit(ReportError('Unsupported property attribute'));
      FIter.SkipWhiteSpace;

      propertyTypeString := FIter.CollectAlphaNumeric;
      FIter.SkipWhiteSpace;

      if propertyTypeString = 'list' then begin
         // element is a list, retrieve list count type
         propertyTypeString := FIter.CollectAlphaNumeric;
         FIter.SkipWhiteSpace;

         listType := PropertyTypeByString(propertyTypeString);
         if listType = plyt_none then
            Exit(ReportErrorFmt('Unsupported property list type "%s"', [ propertyTypeString ]));

         propertyTypeString := FIter.CollectAlphaNumeric;
         FIter.SkipWhiteSpace;
      end else begin
         listType := plyt_none;
      end;

      propertyType := PropertyTypeByString(propertyTypeString);
      if propertyType = plyt_none then
         Exit(ReportErrorFmt('Unsupported property type "%s"', [ propertyTypeString ]));

      propertyName := FIter.CollectAlphaNumericUnderscore;
      if propertyName = '' then
         Exit(ReportError('Missing property name'));
      FIter.SkipWhiteSpace;

      element.AddProperty(propertyName, listType, propertyType);
   end;

   Result := True;
end;

// ------------------
// ------------------ TPLYProperty ------------------
// ------------------

// EndianSwapData
//
procedure TPLYProperty.EndianSwapData(var context : TPLYEndianSwapContext);
begin
   if ListTyp <> plyt_none then begin

      var nbItems : Integer;
      if context.CurrentIsNativeEndian then
         nbItems := GetNativeIntegerFromType(context.Ptr, ListTyp)
      else nbItems := GetSwappedIntegerFromType(context.Ptr, ListTyp);
      context.EndianSwapType(ListTyp);
      for var i := 1 to nbItems do
         context.EndianSwapType(Typ);

   end else begin

      context.EndianSwapType(Typ);

   end;
end;

// ------------------
// ------------------ TPLYElement ------------------
// ------------------

// Destroy
//
destructor TPLYElement.Destroy;
begin
   ClearProperties;
   inherited;
end;

// EndianSwapData
//
procedure TPLYElement.EndianSwapData(currentIsNativeEndian : Boolean);
var
   swapContext : TPLYEndianSwapContext;
begin
   swapContext.Ptr := PByte(Pointer(Data));
   swapContext.BytesRemaining := Length(Data);
   swapContext.CurrentIsNativeEndian := currentIsNativeEndian;
   for var i := 1 to Count do begin
      for var j := 0 to High(FProperties) do
         FProperties[j].EndianSwapData(swapContext);
   end;
   Assert(swapContext.bytesRemaining = 0);
end;

// GetProperties
//
function TPLYElement.GetProperties(index : Integer) : TPLYProperty;
begin
   if Cardinal(index) >= Cardinal(Length(FProperties)) then
      raise EPLYException.CreateFmt('Property index out of bounds (%d)', [ index ]);
   Result := FProperties[index];
end;

// PropertyCount
//
function TPLYElement.PropertyCount : Integer;
begin
   Result := Length(FProperties);
end;

// IndexOfProperty
//
function TPLYElement.IndexOfProperty(const name : String) : Integer;
begin
   for var i := 0 to High(FProperties) do begin
      if FProperties[i].Name = name then Exit(i);
   end;
   Result := -1;
end;

// PropertyNames
//
function TPLYElement.PropertyNames : TStringDynArray;
begin
   SetLength(Result, Length(FProperties));
   for var i := 0 to High(FProperties) do
      Result[i] := FProperties[i].Name;
end;

// HasListProperties
//
function TPLYElement.HasListProperties : Boolean;
begin
   for var prop in FProperties do
      if prop.ListTyp <> plyt_none then
         Exit(True);
   Result := False;
end;

// AddProperty
//
function TPLYElement.AddProperty(const aName : String; aListTyp, aTyp : TPLYPropertyType) : TPLYProperty;
begin
   Result := TPLYProperty.Create;
   Result.FName := aName;
   Result.FListTyp := aListTyp;
   Result.FTyp := aTyp;

   FProperties := FProperties + [ Result ];
end;

// ClearProperties
//
procedure TPLYElement.ClearProperties;
begin
   for var i := 0 to High(FProperties) do
      FProperties[i].Free;
   FProperties := nil;
end;

// ------------------
// ------------------ TModel3DFilePLY ------------------
// ------------------

// Create
//
constructor TModel3DFilePLY.Create;
begin
   inherited;
end;

// Destroy
//
destructor TModel3DFilePLY.Destroy;
begin
   inherited;
   ClearElements;
end;

// LoadFromData
//
function TModel3DFilePLY.LoadFromData(const data : RawByteString; errors : TStrings) : Boolean;
var
   context : TPLYParserContext;
   header, elementsData : String;
begin
   ClearElements;

   context.Errors := errors;

   var pEndOfHeader := PosA('end_header', data);
   if pEndOfHeader <= 0 then
      Exit(context.ReportError('end_header not found'));

   Inc(pEndOfHeader, 10); // length of "end_header"

   BytesToScriptString(Pointer(data), pEndOfHeader, header);

   if not context.ParseHeader(header, Self) then Exit(False);

   while data[pEndOfHeader] in [ #13, #10 ] do
      Inc(pEndOfHeader);

   var remainingBytes := Length(data) - pEndOfHeader + 1;
   case FFormat of
      ply_ascii : begin
         BytesToScriptString(@data[pEndOfHeader], remainingBytes, elementsData);
         Exit(context.ParseData(elementsData, Self));
      end;
      ply_binary_little_endian, ply_binary_big_endian : begin

         var fileEndian : TEndian;
         if FFormat = ply_binary_little_endian then
            fileEndian := TEndian.Little
         else fileEndian := TEndian.Big;
         var currentIsNativeEndian := PlatformEndian = fileEndian;

         var p := PByte(@data[pEndOfHeader]);

         for var element in FElements do begin
            var pElementDataStart := p;
            var nbBytes := 0;
            if element.HasListProperties then begin
               // some property has variable length, need to go through
               for var i := 1 to element.Count do begin
                  for var j := 0 to High(element.FProperties) do begin
                     var prop := element.FProperties[j];
                     case prop.ListTyp of
                        plyt_none : begin
                           Inc(nbBytes, cPLYPropertyByteLength[prop.Typ]);
                        end;
                        plyt_uchar : begin
                           var nbItems := PByte(p)^;
                           Inc(nbBytes,   cPLYPropertyByteLength[prop.ListTyp]
                                        + cPLYPropertyByteLength[prop.Typ] * nbItems);
                        end;
                     else
                        var nbItems : Integer;
                        if currentIsNativeEndian then
                           nbItems := GetNativeIntegerFromType(p, prop.ListTyp)
                        else nbItems := GetSwappedIntegerFromType(p, prop.ListTyp);
                        Inc(nbBytes,   cPLYPropertyByteLength[prop.ListTyp]
                                     + cPLYPropertyByteLength[prop.Typ] * nbItems);
                     end;
                  end;
               end;
            end else begin
               for var prop in element.FProperties do
                  Inc(nbBytes, cPLYPropertyByteLength[prop.Typ]);
               nbBytes := nbBytes * element.Count;
            end;

            if nbBytes > remainingBytes then
               Exit(context.ReportErrorFmt('Out of data on element "%s"', [ element.Name ]));
            Inc(p, nbBytes);
            Dec(remainingBytes, nbBytes);

            SetLength(element.FData, nbBytes);
            if nbBytes > 0 then
               System.Move(pElementDataStart^, element.FData[0], nbBytes);
            if not currentIsNativeEndian then
               element.EndianSwapData(currentIsNativeEndian);
         end;
      end;
   else
      Exit(context.ReportError('Data parsing not implemented for this format'));
   end;
   Result := True;
end;

// GetElements
//
function TModel3DFilePLY.GetElements(index : Integer) : TPLYElement;
begin
   if Cardinal(index) >= Cardinal(Length(FElements)) then
      raise EPLYException.CreateFmt('Element index out of bounds (%d)', [ index ]);
   Result := FElements[index];
end;

// ElementCount
//
function TModel3DFilePLY.ElementCount : Integer;
begin
   Result := Length(FElements);
end;

// ClearElements
//
procedure TModel3DFilePLY.ClearElements;
begin
   for var i := 0 to High(FElements) do
      FElements[i].Free;
   FElements := nil;
end;

end.
