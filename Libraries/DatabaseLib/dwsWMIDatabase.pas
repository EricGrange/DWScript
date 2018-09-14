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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{
   This is a pseudo-database where all queries return an infinite database
   comprised of a single column whose values are GUIDs
}
unit dwsWMIDatabase;

interface

uses
   Classes, SysUtils, ActiveX, ComObj, Variants,
   dwsUtils, dwsDatabase, dwsXPlatform, dwsDataContext, dwsSymbols, dwsJSON;

type

   TdwsWMIDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FService : IDispatch;

      public
         procedure Connect(const host, root : String);

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;
         function CanReleaseToPool : String;

         procedure Exec(const sql : String; const parameters : IDataContext; context : TExprBase);
         function Query(const sql : String; const parameters : IDataContext; context : TExprBase) : IdwsDataSet;

         function VersionInfoText : String;
   end;

   TdwsWMIDataSet = class (TdwsDataSet)
      private
         FQuery : OleVariant;
         FRowsEnum : IEnumVARIANT;
         FCurrentRow : OleVariant;

      protected
         procedure DoPrepareFields; override;

         procedure ReadFieldValues;
         procedure ClearFieldValues;

      public
         procedure Open(const service : IDispatch; const sql : String; const parameters : IDataContext);

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsWMIDataField = class (TdwsDataField)
      protected
         FName : String;
         FValue : OleVariant;
         FCimType : Integer;
         FIsArray : Boolean;

         function GetName : String; override;
         function GetDataType : TdwsDataFieldType; override;
         function GetDeclaredType : String; override;

      public
         function IsNull : Boolean; override;
         procedure AsString(var Result : String); override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
         function AsBlob : RawByteString; override;
   end;

   TdwsWMIBaseFactory = class (TdwsDataBaseFactory)
      public
         function CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// from https://msdn.microsoft.com/en-us/library/aa393974(v=vs.85).aspx
type
   TCimType = (
      cimTypeSINT8      = 16,
      cimTypeUINT8      = 17,
      cimTypeSINT16     = 2,
      cimTypeUINT16     = 18,
      cimTypeSINT32     = 3,
      cimTypeUINT32     = 19,
      cimTypeSINT64     = 20,
      cimTypeUINT64     = 21,
      cimTypeREAL32     = 4,
      cimTypeREAL64     = 5,
      cimTypeBOOLEAN    = 11,
      cimTypeSTRING     = 8,
      cimTypeDATETIME   = 101,
      cimTypeREFERENCE  = 102,
      cimTypeCHAR16     = 103,
      cimTypeOBJECT     = 13
   );

// ------------------
// ------------------ TdwsWMIBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsWMIBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   host, root : String;
   db : TdwsWMIDataBase;
begin
   case Length(parameters) of
      0 : begin
         host := '.';
         root := 'root\cimv2';
      end;
      1 : begin
         host := '.';
         root := parameters[0];
      end;
      2 : begin
         host := parameters[0];
         root := parameters[1];
      end;
   else
      raise Exception.CreateFmt(
         'WMI expected 0 to 2 parameters but got %d', [Length(parameters)]
      );
   end;

   db := TdwsWMIDataBase.Create;
   Result := db;
   db.Connect(host, root);
end;

// ------------------
// ------------------ TdwsWMIDataBase ------------------
// ------------------

// Connect
//
procedure TdwsWMIDataBase.Connect(const host, root : String);
var
   bindCtx : IBindCtx;
   moniker : IMoniker;
   servicePath : String;
   chEaten : Integer;
begin
   inherited Create;

   servicePath := 'winmgmts:\\' + host +'\' + root;

   OleCheck(CreateBindCtx(0, bindCtx));
   OleCheck(MkParseDisplayName(bindCtx, StringToOleStr(servicePath), chEaten, moniker));
   OleCheck(moniker.BindToObject(bindCtx, nil, IDispatch, FService));
end;

// BeginTransaction
//
procedure TdwsWMIDataBase.BeginTransaction;
begin
   raise Exception.Create('Transactions not supported');
end;

// Commit
//
procedure TdwsWMIDataBase.Commit;
begin
   raise Exception.Create('Transactions not supported');
end;

// Rollback
//
procedure TdwsWMIDataBase.Rollback;
begin
   raise Exception.Create('Transactions not supported');;
end;

// InTransaction
//
function TdwsWMIDataBase.InTransaction : Boolean;
begin
   Result := False;
end;

// CanReleaseToPool
//
function TdwsWMIDataBase.CanReleaseToPool : String;
begin
   Result := 'Not poolable';
end;

// Exec
//
procedure TdwsWMIDataBase.Exec(const sql : String; const parameters : IDataContext; context : TExprBase);
begin
   raise Exception.Create('Exec not supported');
end;

// Query
//
function TdwsWMIDataBase.Query(const sql : String; const parameters : IDataContext; context : TExprBase) : IdwsDataSet;
var
   ds : TdwsWMIDataSet;
begin
   ds := TdwsWMIDataSet.Create(Self);
   Result := ds;
   ds.Open(FService, sql, parameters);
end;

// VersionInfoText
//
function TdwsWMIDataBase.VersionInfoText : String;
begin
   Result := 'WMI Database';
end;

// ------------------
// ------------------ TdwsWMIDataSet ------------------
// ------------------

// Open
//
procedure TdwsWMIDataSet.Open(const service : IDispatch; const sql : String; const parameters : IDataContext);
const
   wbemFlagForwardOnly = 32;
begin
   if parameters.DataLength <> 0 then
      raise Exception.Create('Parameters not supported');
   FQuery := OleVariant(service).ExecQuery(sql, 'WQL', wbemFlagForwardOnly);
   FRowsEnum := IUnknown(FQuery._NewEnum) as IEnumVARIANT;
   Next;
end;

// Eof
//
function TdwsWMIDataSet.Eof : Boolean;
begin
   Result := (TVarData(FCurrentRow).VType = varEmpty);
end;

// Next
//
procedure TdwsWMIDataSet.Next;
var
   nb : Cardinal;
begin
   VariantClear(FCurrentRow);
   if FRowsEnum.Next(1, FCurrentRow, nb) <> 0 then
      ClearFieldValues
   else ReadFieldValues;
end;

// FieldCount
//
function TdwsWMIDataSet.FieldCount : Integer;
begin
   Result := Length(FFields);
end;

// DoPrepareFields
//
procedure TdwsWMIDataSet.DoPrepareFields;
begin
   // Handled by ReadFieldValues
end;

// ReadFieldValues
//
procedure TdwsWMIDataSet.ReadFieldValues;
var
   i : Integer;
   enum : IEnumVARIANT;
   prop : OleVariant;
   n : Cardinal;
   field : TdwsWMIDataField;
begin
   enum := IUnknown(FCurrentRow.Properties_._NewEnum) as IEnumVARIANT;

   i := 0;
   while enum.Next(1, prop, n) = 0 do begin
      try
         if i >= Length(FFields) then begin
            SetLength(FFields, i+1);
            field := TdwsWMIDataField.Create(Self, i);
            FFields[i] := field;
         end else begin
            field := TdwsWMIDataField(FFields[i]);
         end;
         field.FName := prop.Name;
         field.FValue := prop.Value;
         field.FCimType := prop.CimType;
         field.FIsArray := prop.IsArray;
         Inc(i);
      finally
         VariantClear(prop);
      end;
   end;
end;

// ClearFieldValues
//
procedure TdwsWMIDataSet.ClearFieldValues;
var
   i : Integer;
begin
   for i := 0 to High(FFields) do
      VariantClear(TdwsWMIDataField(FFields[i]).FValue);
end;

// ------------------
// ------------------ TdwsWMIDataField ------------------
// ------------------

// IsNull
//
function TdwsWMIDataField.IsNull : Boolean;
begin
   Result := VarType(FValue) in [varNull, varEmpty];
end;

// GetName
//
function TdwsWMIDataField.GetName : String;
begin
   Result := FName;
end;

// GetDataType
//
function TdwsWMIDataField.GetDataType : TdwsDataFieldType;
begin
   if not FIsArray then begin
      case TCimType(FCimType) of
         cimTypeSINT8, cimTypeUINT8, cimTypeSINT16, cimTypeUINT16,
         cimTypeSINT32, cimTypeUINT32, cimTypeSINT64, cimTypeUINT64 :
            Result := dftString;
         cimTypeREAL32, cimTypeREAL64 :
            Result := dftFloat;
         cimTypeSTRING, cimTypeCHAR16 :
            Result := dftString;
         cimTypeBOOLEAN :
            Result := dftBoolean;
         cimTypeDATETIME :
            Result := dftDateTime;
      else
         Result := dftUnknown;
      end;
   end else Result := dftUnknown;
end;

// GetDeclaredType
//
function TdwsWMIDataField.GetDeclaredType : String;
begin
   case TCimType(FCimType and $FF) of
      cimTypeSINT8 : Result := 'SInt8';
      cimTypeUINT8 : Result := 'UInt8';
      cimTypeSINT16 : Result := 'SInt16';
      cimTypeUINT16 : Result := 'UInt16';
      cimTypeSINT32 : Result := 'SInt32';
      cimTypeUINT32 : Result := 'UInt32';
      cimTypeSINT64 : Result := 'SInt64';
      cimTypeUINT64 : Result := 'UInt64';
      cimTypeREAL32 : Result := 'Real32';
      cimTypeREAL64 : Result := 'Real64';
      cimTypeBOOLEAN : Result := 'Boolean';
      cimTypeSTRING : Result := 'String';
      cimTypeDATETIME : Result := 'DateTime';
      cimTypeREFERENCE : Result := 'Reference';
      cimTypeCHAR16 :  Result := 'Char16';
      cimTypeOBJECT : Result := 'Object';
   else
      Result := Format('Unknown(%d)', [FCimType]);
   end;
   if FIsArray then
      Result := Result + '[]';
end;

// AsString
//
procedure TdwsWMIDataField.AsString(var Result : String);
var
   w : TdwsJSONWriter;
begin
   if VarIsArray(FValue) then begin
      w := TdwsJSONWriter.Create;
      try
         w.WriteVariant(FValue);
         Result := w.ToString;
      finally
         w.Free;
      end;
   end else Result := VariantToString(FValue);
end;

// AsInteger
//
function TdwsWMIDataField.AsInteger : Int64;
begin
   Result := VariantToInt64(FValue);
end;

// AsFloat
//
function TdwsWMIDataField.AsFloat : Double;
begin
   Result := VariantToFloat(FValue);
end;

// AsBlob
//
function TdwsWMIDataField.AsBlob : RawByteString;
begin
   Result := RawByteString(FValue);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('WMI', TdwsWMIBaseFactory.Create);

end.
