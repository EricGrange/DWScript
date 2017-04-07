unit dwsClasses;

{$i dws.inc}

interface

uses Windows, SysUtils, Classes, dwsUtils, dwsXPlatform;

type

   { TdwsStrings class }

  TStringsDefined = set of (sdDelimiter, sdQuoteChar);

  TdwsStrings = class(TPersistent)
  private
    FDefined: TStringsDefined;
    FDelimiter: WideChar;
    FQuoteChar: WideChar;
    FUpdateCount: Integer;
    function GetCommaText: String;
    function GetDelimitedText: String;
    function GetName(Index: Integer): String;
    function GetValue(const Name: String): String;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: String);
    procedure SetDelimitedText(const Value: String);
    procedure SetValue(const Name, Value: String);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: String): String;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;

    function Get(Index: Integer): String; virtual; abstract;
    procedure Put(Index: Integer; const S: String); virtual;

    function GetObject(Index: Integer): IUnknown; virtual;
    procedure PutObject(Index: Integer; const AObject: IUnknown); virtual;

    procedure SetCapacity(NewCapacity: Integer); virtual;

    function  GetTextStr: String; virtual;
    procedure SetTextStr(const Value: String); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: String): Integer; virtual;

  public
    destructor Destroy; override;
    function Add(const S: String): Integer; virtual;
    function AddObject(const S: String; const AObject: IUnknown): Integer; virtual;
    procedure Append(const S: String);
    procedure AddStrings(Strings: TdwsStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Remove(const str : String);
    procedure EndUpdate;
    function  Equals(Strings: TdwsStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function  IndexOf(const S: String): Integer; virtual;
    function  IndexOfName(const Name: String): Integer; virtual;
    function  IndexOfObject(const AObject: IUnknown): Integer; virtual;
    procedure Insert(Index: Integer; const S: String); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: String; const AObject: IUnknown); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: String read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: String read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: String read GetName;
    property Objects[Index: Integer]: IUnknown read GetObject write PutObject;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property Values[const Name: String]: String read GetValue write SetValue;
    property Strings[Index: Integer]: String read Get write Put; default;
    property Text: String read GetTextStr write SetTextStr;
  end;

{ TdwsStringList class }

  TdwsStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString : String;
    FObject : IUnknown;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxInt shr 5] of TStringItem;
  TStringListSortCompare = function(List: TdwsStringList; Index1, Index2: Integer): Integer;

  TdwsStringList = class(TdwsStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);

  protected
    function Get(Index: Integer): String; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): IUnknown; override;
    procedure Put(Index: Integer; const S: String); override;
    procedure PutObject(Index: Integer; const AObject: IUnknown); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    function CompareStrings(const S1, S2: String): Integer; override;
    procedure InsertItem(Index: Integer; const S: String; const AObject: IUnknown); virtual;

  public
    destructor Destroy; override;

    function  Add(const S: String): Integer; override;
    function  AddObject(const S: String; const AObject: IUnknown): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function  Find(const S: String; var Index: Integer): Boolean; virtual;
    function  IndexOf(const S: String): Integer; override;
    function  IndexOfObject(const AObject: IUnknown): Integer; override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertObject(Index: Integer; const S: String; const AObject: IUnknown); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
  end;

implementation

{$IFDEF MSWINDOWS}
uses RTLConsts, Types;
{$ENDIF}
{$IFDEF LINUX}
uses RTLConsts, SysConst;
{$ENDIF}

{ TdwsStrings }

destructor TdwsStrings.Destroy;
begin
  inherited Destroy;
end;

function TdwsStrings.Add(const S: String): Integer;
begin
   Result := GetCount;
   Insert(Result, S);
end;

function TdwsStrings.AddObject(const S: String; const AObject: IUnknown): Integer;
begin
   Result := Add(S);
   PutObject(Result, AObject);
end;

procedure TdwsStrings.Append(const S: String);
begin
   Add(S);
end;

procedure TdwsStrings.AddStrings(Strings: TdwsStrings);
var
  I: Integer;
begin
   BeginUpdate;
   try
      for I := 0 to Strings.Count - 1 do
         AddObject(Strings[I], Strings.Objects[I]);
   finally
      EndUpdate;
   end;
end;

procedure TdwsStrings.Assign(Source: TPersistent);
begin
  if Source is TdwsStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TdwsStrings(Source).FDefined;
      FQuoteChar := TdwsStrings(Source).FQuoteChar;
      FDelimiter := TdwsStrings(Source).FDelimiter;
      AddStrings(TdwsStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TdwsStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TdwsStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TdwsStrings then
        Result := not Equals(TdwsStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TdwsStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TdwsStrings.Equals(Strings: TdwsStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TdwsStrings.Error(const Msg: String; Data: Integer);
{$ifdef FPC}
begin
   raise EStringListError.Create(Msg);
{$else}
  function ReturnAddr: Pointer;
  asm
     MOV eax, [EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
{$endif}
end;

procedure TdwsStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TdwsStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: IUnknown;
  TempString: String;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TdwsStrings.ExtractName(const S: String): String;
var
  P: Integer;
begin
  Result := S;
  P := Pos('=', Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TdwsStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;

function TdwsStrings.GetCommaText: String;
var
  FOldDefined: TStringsDefined;
  FOldDelimiter: Char;
  FOldQuoteChar: Char;
begin
  FOldDefined := FDefined;
  FOldDelimiter := FDelimiter;
  FOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := FOldDelimiter;
    FQuoteChar := FOldQuoteChar;
    FDefined := FOldDefined;
  end;
end;

function TdwsStrings.GetDelimitedText: String;
var
  S: String;
  P: PChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while (P^>' ') and (P^<>QuoteChar) and (P^<>Delimiter) do
        Inc(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TdwsStrings.GetName(Index: Integer): String;
begin
  Result := ExtractName(Get(Index));
end;

function TdwsStrings.GetObject(Index: Integer): IUnknown;
begin
  Result := nil;
end;

function TdwsStrings.GetTextStr: String;
var
   I, L, Size, Count: Integer;
   P: PWideChar;
   S, LB: String;
begin
   Count := GetCount;
   Size := 0;
   LB := sLineBreak;
   for I := 0 to Count - 1 do
      Inc(Size, Length(Get(I)) + Length(LB));
   SetString(Result, nil, Size);
   P := Pointer(Result);
   for I := 0 to Count - 1 do begin
      S := Get(I);
      L := Length(S);
      if L <> 0 then begin
         System.Move(Pointer(S)^, P^, L*SizeOf(Char));
         Inc(P, L);
      end;
      L := Length(LB);
      if L <> 0 then begin
         System.Move(Pointer(LB)^, P^, L*SizeOf(Char));
         Inc(P, L);
      end;
   end;
end;

function TdwsStrings.GetValue(const Name: String): String;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TdwsStrings.IndexOf(const S: String): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TdwsStrings.IndexOfName(const Name: String): Integer;
var
  P: Integer;
  S: String;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TdwsStrings.IndexOfObject(const AObject: IUnknown): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TdwsStrings.InsertObject(Index: Integer; const S: String; const AObject: IUnknown);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TdwsStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: String;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size*SizeOf(Char));
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TdwsStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: IUnknown;
  TempString: String;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdwsStrings.Put(Index: Integer; const S: String);
var
  TempObject: IUnknown;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TdwsStrings.PutObject(Index: Integer; const AObject: IUnknown);
begin
end;

procedure TdwsStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(String(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TdwsStrings.SaveToStream(Stream: TStream);
var
  S: String;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S)*SizeOf(Char));
end;

procedure TdwsStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TdwsStrings.SetCommaText(const Value: String);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TdwsStrings.SetTextStr(const Value: String);
var
  P, Start: PWideChar;
  S: String;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TdwsStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TdwsStrings.SetValue(const Name, Value: String);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TdwsStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(String(Get(I)));
  Writer.WriteListEnd;
end;

function AnsiExtractQuotedStrW(var src: PWideChar; quote: WideChar): WideString;
var
   p, q, r : PWideChar;
begin
   p := src;
   q := StrEnd(p);
   result:='';
   if p = q then Exit;
   if p^ <> quote then Exit;
   Inc(p);

   SetLength(Result, (NativeUInt(q)-NativeUInt(p))+1);
   r := @Result[1];
   while p <> q do begin
      r^ := p^;
      Inc(r);
      if (p^ = quote) then begin
         Inc(p);
         if p^ <> quote then begin
            dec(r);
            break;
         end;
      end;
      Inc(p);
   end ;
   src:=p;
   SetLength(result, NativeUInt(r)-NativeUInt(Pointer(Result)));
end ;

procedure TdwsStrings.SetDelimitedText(const Value: String);
var
  P, P1: PChar;
  S: String;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while CharInSet(P^, [#1..' ']) do
      Inc(P);
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while CharInSet(P^, [#1..' ']) do
        Inc(P);
      if P^ = Delimiter then
      begin
        P1 := P;
        Inc(P1);
        if P1^ = #0 then
           Add('');
        repeat
          Inc(P);
        until not CharInSet(P^, [#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TdwsStrings.GetDelimiter: Char;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TdwsStrings.GetQuoteChar: Char;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TdwsStrings.SetDelimiter(const Value: Char);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TdwsStrings.SetQuoteChar(const Value: Char);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

function TdwsStrings.CompareStrings(const S1, S2: String): Integer;
begin
  Result := UnicodeCompareText(S1, S2);
end;

// Remove
//
procedure TdwsStrings.Remove(const str : String);
var
   i : Integer;
begin
   i:=IndexOf(str);
   if i>=0 then
      Delete(i);
end;

{ TdwsStringList }

destructor TdwsStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TdwsStringList.Add(const S: String): Integer;
begin
  Result := AddObject(S, nil);
end;

function TdwsStringList.AddObject(const S: String; const AObject: IUnknown): Integer;
begin
   if not Sorted then
      Result := FCount
   else begin
      if Find(S, Result) then begin
         case Duplicates of
            dupIgnore: Exit;
            dupError: Error(@SDuplicateString, 0);
         end;
      end;
   end;
   InsertItem(Result, S, AObject);
end;

procedure TdwsStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

procedure TdwsStringList.Delete(Index: Integer);
begin
  if Cardinal(Index)>=Cardinal(FCount) then Error(@SListIndexError, Index);
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TStringItem));
end;

procedure TdwsStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  ExchangeItems(Index1, Index2);
end;

procedure TdwsStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

function TdwsStringList.Find(const S: String; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TdwsStringList.Get(Index: Integer): String;
begin
   if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
   Result := FList^[Index].FString;
end;

function TdwsStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TdwsStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TdwsStringList.GetObject(Index: Integer): IUnknown;
begin
   if Cardinal(Index) >= Cardinal(FCount) then Error(@SListIndexError, Index);
   Result := FList^[Index].FObject;
end;

procedure TdwsStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TdwsStringList.IndexOf(const S: String): Integer;
begin
   if not Sorted then begin
      for Result := 0 to GetCount - 1 do
         if CompareStrings(FList[Result].FString, S) = 0 then Exit;
      Result := -1;
   end else if not Find(S, Result) then
      Result := -1;
end;

// IndexOfObject
//
function TdwsStringList.IndexOfObject(const AObject: IUnknown): Integer;
begin
   for Result:=0 to FCount - 1 do
      if FList[Result].FObject=AObject then Exit;
   Result := -1;
end;

procedure TdwsStringList.Insert(Index: Integer; const S: String);
begin
  InsertObject(Index, S, nil);
end;

procedure TdwsStringList.InsertObject(Index: Integer; const S: String; const AObject: IUnknown);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TdwsStringList.InsertItem(Index: Integer; const S: String; const AObject: IUnknown);
begin
   if FCount = FCapacity then Grow;
   if Index < FCount then
      System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TStringItem));
   with FList^[Index] do begin
      Pointer(FString) := nil;
      FObject := AObject;
      FString := S;
   end;
   Inc(FCount);
end;

procedure TdwsStringList.Put(Index: Integer; const S: String);
begin
  if Sorted then Error(@SSortedListError, 0);
  if Cardinal(Index)>=Cardinal(FCount) then Error(@SListIndexError, Index);
  FList^[Index].FString := S;
end;

procedure TdwsStringList.PutObject(Index: Integer; const AObject: IUnknown);
begin
   if Cardinal(Index)>=Cardinal(FCount) then Error(@SListIndexError, Index);
   FList^[Index].FObject := AObject;
end;

procedure TdwsStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TdwsStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCapacity then
    FillChar(FList[NewCapacity], (FCapacity - NewCapacity) * SizeOf(TStringItem), 0);

  ReallocMem(FList, NewCapacity * SizeOf(TStringItem));

  if NewCapacity > FCapacity then
    FillChar(FList[FCapacity], (NewCapacity - FCapacity) * SizeOf(TStringItem), 0);
    
  FCapacity := NewCapacity;
end;

procedure TdwsStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

function StringListCompareStrings(List: TdwsStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TdwsStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TdwsStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
    QuickSort(0, FCount - 1, Compare);
end;

function TdwsStringList.CompareStrings(const S1, S2: String): Integer;
begin
  if CaseSensitive then
    Result := UnicodeCompareStr(S1, S2)
  else
    Result := UnicodeCompareText(S1, S2);
end;

procedure TdwsStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

end.
