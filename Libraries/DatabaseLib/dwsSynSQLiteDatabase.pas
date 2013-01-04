unit dwsSynSQLiteDatabase;

interface

uses
   Classes, Variants, SysUtils,
   SynSQLite3,
   dwsUtils, dwsExprs, dwsDatabase, dwsStack, dwsXPlatform;

type

   TdwsSynSQLiteDataBase = class (TdwsDataBase, IdwsDataBase)
      private
         FDB : TSQLDatabase;

      public
         constructor Create(const parameters : array of String); override;
         destructor Destroy; override;

         procedure Close;

         procedure BeginTransaction;
         procedure Commit;
         procedure Rollback;
         function InTransaction : Boolean;

         procedure Exec(const sql : String; const parameters : TData);
         function Query(const sql : String; const parameters : TData) : IdwsDataSet;
   end;

   TdwsSynSQLiteDataSet = class (TdwsDataSet)
      private
         FDB : TdwsSynSQLiteDataBase;
         FQuery : TSQLRequest;
         FEOFReached : Boolean;

      protected
         procedure DoPrepareFields; override;

      public
         constructor Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : TData);
         destructor Destroy; override;

         function Eof : Boolean; override;
         procedure Next; override;

         function FieldCount : Integer; override;
   end;

   TdwsSynSQLiteDataField = class (TdwsDataField)
      protected
         function GetName : String; override;
         function GetDataType : String; override;

      public
         constructor Create(dataSet : TdwsSynSQLiteDataSet; fieldIndex : Integer);

         function IsNull : Boolean; override;
         function AsString : String; override;
         function AsInteger : Int64; override;
         function AsFloat : Double; override;
   end;

//   IdwsBlob = interface
//      ['{018C9441-3177-49E1-97EF-EA5F2584FA60}']
//   end;

   TdwsSynSQLiteDataBaseFactory = class (TdwsDataBaseFactory)
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

procedure AssignParameters(var rq : TSQLRequest; const params : TData);
var
   i : Integer;
   p : PVarData;
begin
   for i:=1 to Length(params) do begin
      p:=PVarData(@params[i-1]);
      case p.VType of
         varInt64 : rq.Bind(i, p.VInt64);
         varDouble : rq.Bind(i, p.VDouble);
         varUString : rq.BindS(i, String(p.VUString));
         varBoolean : rq.Bind(i, Ord(p.VBoolean));
         varNull : rq.BindNull(i);
      else
         raise Exception.CreateFmt('Unsupported VarType %d', [p.VType]);
      end;
   end;
end;

// ------------------
// ------------------ TdwsSynSQLiteDataBaseFactory ------------------
// ------------------

// CreateDataBase
//
function TdwsSynSQLiteDataBaseFactory.CreateDataBase(const parameters : TStringDynArray) : IdwsDataBase;
var
   db : TdwsSynSQLiteDataBase;
begin
   db:=TdwsSynSQLiteDataBase.Create(parameters);
   Result:=db;
end;

// ------------------
// ------------------ TdwsSynSQLiteDataBase ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataBase.Create(const parameters : array of String);
var
   dbName : String;
begin
   if Length(parameters)>0 then
      dbName:=TdwsDataBase.ApplyPathVariables(parameters[0])
   else dbName:=':memory:';
   try
      FDB:=TSQLDatabase.Create(dbName);
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataBase.Destroy;
begin
   FDB.Free;
   inherited;
end;

// Close
//
procedure TdwsSynSQLiteDataBase.Close;
begin
   FDB.DBClose;
end;

// BeginTransaction
//
procedure TdwsSynSQLiteDataBase.BeginTransaction;
begin
   FDB.TransactionBegin;
end;

// Commit
//
procedure TdwsSynSQLiteDataBase.Commit;
begin
   FDB.Commit;
end;

// Rollback
//
procedure TdwsSynSQLiteDataBase.Rollback;
begin
   FDB.Rollback;
end;

// InTransaction
//
function TdwsSynSQLiteDataBase.InTransaction : Boolean;
begin
   Result:=FDB.TransactionActive;
end;

// Exec
//
procedure TdwsSynSQLiteDataBase.Exec(const sql : String; const parameters : TData);
var
   rq : TSQLRequest;
begin
   rq.Prepare(FDB.DB, UTF8Encode(sql));
   try
      AssignParameters(rq, parameters);
      rq.Execute;
   except
      rq.Close;
      raise;
   end;
end;

// Query
//
function TdwsSynSQLiteDataBase.Query(const sql : String; const parameters : TData) : IdwsDataSet;
var
   ds : TdwsSynSQLiteDataSet;
begin
   ds:=TdwsSynSQLiteDataSet.Create(Self, sql, parameters);
   Result:=ds;
end;

// ------------------
// ------------------ TdwsSynSQLiteDataSet ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataSet.Create(db : TdwsSynSQLiteDataBase; const sql : String; const parameters : TData);
begin
   FDB:=db;
   inherited Create(db);
   try
      FQuery.Prepare(db.FDB.DB, UTF8Encode(sql));
      try
         AssignParameters(FQuery, parameters);
         FEOFReached:=(FQuery.Step=SQLITE_DONE);
      except
         FQuery.Close;
         raise;
      end;
   except
      RefCount:=0;
      raise;
   end;
end;

// Destroy
//
destructor TdwsSynSQLiteDataSet.Destroy;
begin
   if FQuery.Request<>0 then
      FQuery.Close;
   inherited;
end;

// Eof
//
function TdwsSynSQLiteDataSet.Eof : Boolean;
begin
   Result:=FEOFReached;
end;

// Next
//
procedure TdwsSynSQLiteDataSet.Next;
begin
   FEOFReached:=(FQuery.Step=SQLITE_DONE);
end;

// FieldCount
//
function TdwsSynSQLiteDataSet.FieldCount : Integer;
begin
   Result:=FQuery.FieldCount;
end;

// DoPrepareFields
//
procedure TdwsSynSQLiteDataSet.DoPrepareFields;
var
   i, n : Integer;
begin
   n:=FQuery.FieldCount;
   SetLength(FFields, n);
   for i:=0 to n-1 do
      FFields[i]:=TdwsSynSQLiteDataField.Create(Self, i);
end;

// ------------------
// ------------------ TdwsSynSQLiteDataField ------------------
// ------------------

// Create
//
constructor TdwsSynSQLiteDataField.Create(dataSet : TdwsSynSQLiteDataSet; fieldIndex : Integer);
begin
   inherited Create(dataSet, fieldIndex);
end;

// IsNull
//
function TdwsSynSQLiteDataField.IsNull : Boolean;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldNull(Index);
end;

// GetName
//
function TdwsSynSQLiteDataField.GetName : String;
begin
   Result:=UTF8ToString(TdwsSynSQLiteDataSet(DataSet).FQuery.FieldName(Index));
end;

// GetDataType
//
function TdwsSynSQLiteDataField.GetDataType : String;
const
   cTypeToString : array [SQLITE_INTEGER..SQLITE_NULL] of String = (
      'Integer', 'Float', 'Text', 'Blob', 'Null'
   );
begin
   Result:=cTypeToString[TdwsSynSQLiteDataSet(DataSet).FQuery.FieldType(Index)];
end;

// AsString
//
function TdwsSynSQLiteDataField.AsString : String;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldS(Index);
end;

// AsInteger
//
function TdwsSynSQLiteDataField.AsInteger : Int64;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldInt(Index);
end;

// AsFloat
//
function TdwsSynSQLiteDataField.AsFloat : Double;
begin
   Result:=TdwsSynSQLiteDataSet(DataSet).FQuery.FieldDouble(Index);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsDatabase.RegisterDriver('SQLite', TdwsSynSQLiteDataBaseFactory.Create);

end.
