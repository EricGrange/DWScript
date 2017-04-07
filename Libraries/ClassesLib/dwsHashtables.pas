unit dwsHashtables;

interface

uses SysUtils, dwsXPlatform, dwsUtils;

type
  ValueType = IUnknown;

  THashItem = class
    Twin: THashItem;
    Value: ValueType;
    function HashCode: Integer; virtual; abstract;
    function Equals(Item: THashItem): Boolean; reintroduce; virtual; abstract;
  end;

  PHashItems = ^THashItems;
  THashItems = array[0..MaxInt div Sizeof(THashItem) - 1] of THashItem;

  THashTable = class
  public
    FCapacity: Integer;
    FSize: Integer;
    FThreshold: Integer;
    FLoadFactor: Integer; // In percent
    FInitCapacity: Integer;
    FItems: PHashItems;
    procedure Rehash(NewCapacity: Integer);
  protected
    function InternalGet(Item: THashItem): ValueType;
    procedure InternalPut(Item: THashItem);
    function InternalHasKey(Item: THashItem): Boolean;
    function InternalRemoveKey(Item: THashItem): ValueType;
  public
    constructor Create(InitCapacity: Integer = 256; LoadFactor: Integer = 75);
    destructor Destroy; override;
    procedure Clear;
    property Capacity: Integer read FCapacity;
    property Size: Integer read FSize;
  end;

   TStringHashItem = class(THashItem)
      private
         Key : String;

      public
         function HashCode : Integer; override;
         function Equals(Item : THashItem): Boolean; override;
  end;

  TStringHashTable = class(THashTable)
  private
    FTestItem: TStringHashItem;
  public
    constructor Create(InitCapacity: Integer = 256; LoadFactor: Integer = 75);
    destructor Destroy; override;
    function Get(const Key: String): ValueType;
    procedure Put(const Key: String; Value: ValueType);
    function HasKey(const Key: String): Boolean;
    function RemoveKey(const Key: String): ValueType;
  end;

   TIntegerHashItem = class(THashItem)
      private
         Key: Integer;

      public
         function HashCode: Integer; override;
         function Equals(Item: THashItem): Boolean; override;
  end;

  TIntegerHashTable = class(THashTable)
  private
    FTestItem: TIntegerHashItem;
  public
    constructor Create(InitCapacity: Integer = 256; LoadFactor: Integer = 75);
    destructor Destroy; override;
    function Get(const Key: Integer): ValueType;
    procedure Put(const Key: Integer; Value: ValueType);
    function HasKey(const Key: Integer): Boolean;
    function RemoveKey(const Key: Integer): ValueType;
  end;

implementation

var
  HashTable: array[#0..#255] of Byte;
  InsensitiveHashTable: array[#0..#255] of Word;

procedure InitTables;
var
  I, K: Char;
  Temp: Integer;
begin
  for I := #0 to #255 do
  begin
    HashTable[I] := Ord(I);
    InsensitiveHashTable[I] := Ord(AnsiUpperCase(Char(I))[1]);
  end;
  RandSeed := 111;
  for I := #1 to #255 do
  begin
    repeat
      K := Char(Random(255));
    until K <> #0;
    Temp := HashTable[I];
    HashTable[I] := HashTable[K];
    HashTable[K] := Temp;
  end;
end;

{ THashTable }

constructor THashTable.Create(InitCapacity, LoadFactor: Integer);
begin
  if (InitCapacity < 1) or (InitCapacity >= MaxInt div Sizeof(Integer)) then
    raise Exception.CreateFmt('Invalid InitCapacity: %d', [InitCapacity]);
  if (LoadFactor < 0) or (LoadFactor > 100) then
    raise Exception.CreateFmt('Invalid LoadFactor: %d', [LoadFactor]);
  FInitCapacity:=InitCapacity;
  FLoadFactor := LoadFactor;
  Rehash(InitCapacity);
end;

destructor THashTable.Destroy;
begin
  Clear;
  FreeMem(FItems);
  inherited;
end;

procedure THashTable.Clear;
var
  x: Integer;
  oldItem, hashItem: THashItem;
begin
  for x := 0 to FCapacity - 1 do
  begin
    hashItem := FItems[x];
    while Assigned(hashItem) do
    begin
      oldItem := hashItem;
      hashItem := hashItem.Twin;
      oldItem.Free;
    end;
    FItems[x] := nil;
  end;
  FSize:=0;
  Rehash(FInitCapacity);
end;

function THashTable.InternalGet(Item: THashItem): ValueType;
var
  hashItem: THashItem;
begin
  hashItem := FItems[Item.HashCode mod FCapacity];

  while hashItem <> nil do
  begin
    if hashItem.Equals(Item) then
    begin
      Result := hashItem.Value;
      Exit;
    end;
    hashItem := hashItem.Twin;
  end;

  Result := nil;
end;

function THashTable.InternalHasKey(Item: THashItem): Boolean;
var
  hashItem: THashItem;
begin
  Result := false;

  hashItem := FItems[Item.HashCode mod FCapacity];

  while hashItem <> nil do
  begin
    if hashItem.Equals(Item) then
    begin
      Result := True;
      Exit;
    end;
    hashItem := hashItem.Twin;
  end;
end;

procedure THashTable.InternalPut(Item: THashItem);
var
  hash: Integer;
begin
  Inc(FSize);

  if FSize > FThreshold then
    // Double the size of the hashtable
    Rehash(FCapacity * 2);

  // Find item with same hash-key
  // Insert new item to the existing (if any)
  hash := Item.HashCode mod FCapacity;
  Item.Twin := FItems[hash];
  FItems[hash] := Item;
end;

function THashTable.InternalRemoveKey(Item: THashItem): ValueType;
var
  hashItem, lastItem: THashItem;
  hash: Integer;
begin
  hash := Item.HashCode mod FCapacity;
  hashItem := FItems[hash];
  lastItem := nil;

  while hashItem <> nil do
  begin
    if hashItem.Equals(Item) then
    begin
      // Remove item from pointer chain
      if lastItem = nil then
        FItems[hash] := hashItem.Twin
      else
        lastItem.Twin := hashItem.Twin;

      // Dispose item
      Result := hashItem.Value;
      hashItem.Free;
      Dec(FSize);
      Exit;
    end;
    lastItem := hashItem;
    hashItem := hashItem.Twin;
  end;
  Result := nil;
end;

procedure THashTable.Rehash(NewCapacity: Integer);
var
  x, hash: Integer;
  newItems: PHashItems;
  itm, Twin: THashItem;
begin
  // Enlarge the size of the hashtable
  GetMem(newItems, Sizeof(THashItem) * NewCapacity);

  // Clear new space
  for x := 0 to NewCapacity - 1 do
    newItems[x] := nil;

  // Transfer items to the new hashtable
  for x := 0 to FCapacity - 1 do
  begin
    itm := FItems[x];
    while itm <> nil do
    begin
      Twin := itm.Twin;
      hash := itm.HashCode mod NewCapacity;
      itm.Twin := newItems[hash];
      newItems[hash] := itm;
      itm := Twin;
    end;
  end;

  FreeMem(FItems);

  FItems := newItems;
  FThreshold := (NewCapacity div 100) * FLoadFactor;

  FCapacity := NewCapacity;
end;

{ TStringHashItem }

function TStringHashItem.Equals(Item: THashItem): Boolean;
begin
  Result := UnicodeSameText(Key, TStringHashItem(Item).Key);
end;

function TStringHashItem.HashCode: Integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(Key) do
  begin
    Result := (Result shr 4) xor (((Result xor InsensitiveHashTable[Key[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (ord(InsensitiveHashTable[Key[I]]) shr 4)) and $F) * $80);
    if I = 3 then break;
  end;
  if Result = 0 then Result := Length(Key) mod 8 + 1;
end;

{ TStringHashTable }

constructor TStringHashTable.Create(InitCapacity, LoadFactor: Integer);
begin
  inherited;
  FTestItem := TStringHashItem.Create;
end;

destructor TStringHashTable.Destroy;
begin
  inherited;
  FTestItem.Free;
end;

function TStringHashTable.Get(const Key: String): ValueType;
begin
  FTestItem.Key := Key;
  Result := InternalGet(FTestItem);
end;

function TStringHashTable.HasKey(const Key: String): Boolean;
begin
  FTestItem.Key := Key;
  Result := InternalHasKey(FTestItem);
end;

procedure TStringHashTable.Put(const Key: String; Value: ValueType);
var
  item: TStringHashItem;
begin
  item := TStringHashItem.Create;
  item.Key := Key;
  item.Value := Value;
  InternalPut(item);
end;

function TStringHashTable.RemoveKey(const Key: String): ValueType;
begin
  FTestItem.Key := Key;
  Result := InternalRemoveKey(FTestItem);
end;

{ TIntegerHashItem }

function TIntegerHashItem.Equals(Item: THashItem): Boolean;
begin
  Result := Key = TIntegerHashItem(Item).Key;
end;

function TIntegerHashItem.HashCode: Integer;
begin
  Result := Key;
end;

{ TIntegerHashTable }

constructor TIntegerHashTable.Create(InitCapacity, LoadFactor: Integer);
begin
  inherited;
  FTestItem := TIntegerHashItem.Create;
end;

destructor TIntegerHashTable.Destroy;
begin
  FTestItem.Free;
  inherited;
end;

function TIntegerHashTable.Get(const Key: Integer): ValueType;
begin
  FTestItem.Key := Key;
  Result := InternalGet(FTestItem);
end;

function TIntegerHashTable.HasKey(const Key: Integer): Boolean;
begin
  FTestItem.Key := Key;
  Result := InternalHasKey(FTestItem);
end;

procedure TIntegerHashTable.Put(const Key: Integer; Value: ValueType);
var
  item: TIntegerHashItem;
begin
  item := TIntegerHashItem.Create;
  item.Key := Key;
  item.Value := Value;
  InternalPut(item);
end;

function TIntegerHashTable.RemoveKey(const Key: Integer): ValueType;
begin
  FTestItem.Key := Key;
  Result := InternalRemoveKey(FTestItem);
end;

initialization
  InitTables
end.
