unit Aurelius.Bind.Dataset;

{$I Aurelius.inc}

interface

uses
  Generics.Collections, Classes, DB, SysUtils, TypInfo,
  Aurelius.Criteria.Base,
  Aurelius.Engine.ObjectManager,
  Aurelius.Mapping.Optimization,
  Aurelius.Mapping.Explorer;

type
  TAureliusEntityField = class(TVariantField)
  private
    {$IFDEF NEXTGEN}
    FObject: TObject;
    {$ENDIF}
    function GetAsObject: TObject;
    procedure SetAsObject(const Value: TObject);
  protected
    {$IFDEF DELPHIXE3_LVL}
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
    {$ENDIF}
  public
    property AsObject: TObject read GetAsObject write SetAsObject;
    function AsEntity<T: class>: T;
  end;

  TDatasetCreateObjectEvent = procedure(Dataset: TDataset; var NewObject: TObject) of object;
  TDatasetObjectEvent = procedure(Dataset: TDataset; AObject: TObject) of object;

  {$IFDEF DELPHIXE3_LVL}
  TAureliusValueBuffer = TValueBuffer;
  TAureliusBookmark = TBookmark;
  {$ELSE}
  TAureliusValueBuffer = Pointer;
  TAureliusBookmark = Pointer;
  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  TAureliusRecordBuffer = TRecBuf;
  {$ELSE}
  TAureliusRecordBuffer = TRecordBuffer;
  {$ENDIF}

  TBaseAureliusDataset = class(TDataset)
  strict private
    type
      ValueType = Variant;
      PRecInfo = ^TRecInfo;

      TRecInfo = packed record
        Index: integer;
        Obj: TObject;
        BookmarkFlag: TBookmarkFlag;
      end;

      PValueList = ^TValueList;
      TValueList = array[0..0] of ValueType;
  strict private
    FSourceList: IObjectList;
    FSourceCursor: ICriteriaCursor;
    FSourceCriteria: TCriteria;

    // Variables used for paged cursor
    FRemainingRows: integer;
    FCurrentFirstRow: integer;
    FPageSize: integer;

    FCurrent: integer;
    FIsOpen: boolean;
    FRecBufSize: integer;
    FRecInfoOffset: integer;
    FRecordSize: integer;
    FOldValueBuffer: TAureliusRecordBuffer;
    FFilterBuffer: TAureliusRecordBuffer;
    FModifiedFields: TList<TField>;
    FObjectClass: TClass;
    FInternalList: TList<TObject>;
    FOnObjectInsert: TDatasetObjectEvent;
    FOnCreateObject: TDatasetCreateObjectEvent;
    FOnObjectRemove: TDatasetObjectEVent;
    FOnObjectUpdate: TDatasetObjectEvent;
    procedure SplitProp(const AText: string; var AProp, ASubProp: string);
    function ListCount: integer;
    function GetBufferValueList(Buffer: TAureliusRecordBuffer): PValueList;
    function GetPropValue(APropName: string; Obj: TObject): ValueType;
    procedure SetPropValue(APropName: string; Obj: TObject; Value: ValueType);
    function FindPropInfoByName(Clazz: TClass; APropName: string): TRttiOptimization;
    function CreateObject: TObject;
    function IsSelfField(Field: TField): boolean;
    function GetBufferRecInfo(Buffer: TAureliusRecordBuffer): PRecInfo;
    procedure UpdateListFromParent(Field: TDatasetField);
    function InternalGetRecord(Buffer: TAureliusRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
    procedure FetchAllRecords;
    function FetchMoreRecords: boolean;
    function FetchingRecords: boolean;
    procedure GetTheFieldList(List: TList<TField>; const FieldNames: string);
    function GetFieldVariant(Field: TField; var Data: ValueType): boolean;
  private
    {$IFDEF DELPHIXE4_LVL}
    const NullBuffer: TAureliusRecordBuffer = 0;
    {$ELSE}
    const NullBuffer: TAureliusRecordBuffer = nil;
    {$ENDIF}
  private
    function IsEnumeration(ATypeInfo: PTypeInfo): boolean;
    function GetActiveRecBuf: TAureliusRecordBuffer;
    class function ObjectToVariant(Obj: TObject): Variant;
    class function VariantToObject(V: Variant): TObject;
    procedure SetObjectClass(const Value: TClass);
    function GetInternalList: IReadOnlyObjectList;
    function GetBlobData(Field: TField): TBytes;
  protected
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean; ResultFields: string): Boolean; virtual;
  protected
    procedure CreateFields; override;
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;
    {$IFDEF DELPHIXE2_LVL}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    {$ENDIF}
    function GetCanModify: boolean; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetRecordCount: integer; override;
    function GetRecNo: Longint; override;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure SetRecNo(Value: integer); override;
    procedure DoAfterOpen; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
  protected
    {$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ELSE}
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    {$ENDIF}

    procedure GetBookmarkData(Buffer: TAureliusRecordBuffer; Data: TAureliusBookmark); overload; override;
    function GetBookmarkFlag(Buffer: TAureliusRecordBuffer): TBookmarkFlag; override;
    function GetRecordSize: Word; override;
//    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); overload; virtual;
//    procedure InternalAddRecord(Buffer: Pointer; Append: boolean); override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: TAureliusBookmark); override;
    procedure InternalInitRecord(Buffer: TAureliusRecordBuffer); override;
    {$IFDEF DELPHIXE4_LVL}
    {$IFNDEF NEXTGEN}
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    {$ENDIF}
    {$ENDIF}
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TAureliusRecordBuffer); override;
    procedure SetBookmarkFlag(Buffer: TAureliusRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TAureliusRecordBuffer; Data: TAureliusBookmark); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TAureliusValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TAureliusValueBuffer; NativeFormat: boolean); override;
  protected
    function GetRecord(Buffer: TAureliusRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
  protected
    function GetExplorer: TMappingExplorer; virtual;
    procedure InternalSetSourceList(SourceList: TObject);
    function DoCreateObject: TObject; virtual;
    procedure DoObjectRemove(Obj: TObject); virtual;
    procedure DoObjectInsert(Obj: TObject); virtual;
    procedure DoObjectUpdate(Obj: TObject); virtual;
    procedure InternalObjectRemove(Obj: TObject); virtual;
    procedure InternalObjectInsert(Obj: TObject); virtual;
    procedure InternalObjectUpdate(Obj: TObject); virtual;
    procedure InitFieldDefsFromClass(AClass: TClass);
    property ObjectView default True;
    property InternalList: IReadOnlyObjectList read GetInternalList;
    property Explorer: TMappingExplorer read GetExplorer;
  public
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    {$IFDEF DELPHIXE4_LVL}
    function GetFieldData(Field: TField; var Buffer: TAureliusValueBuffer): boolean; override;
    {$ELSE}
    function GetFieldData(Field: TField; Buffer: TAureliusValueBuffer): boolean; override;
    {$ENDIF}
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Current<T: class>: T;
    procedure SetSourceList(SourceList: TObject);
    procedure SetSourceObject(SourceObject: TObject);
    procedure SetSourceCursor(Cursor: ICriteriaCursor);
    procedure SetSourceCriteria(Criteria: TCriteria); overload;
    procedure SetSourceCriteria(Criteria: TCriteria; PageSize: integer); overload;
    function EntityFieldByName(const FieldName: string): TAureliusEntityField;
    property ObjectClass: TClass read FObjectClass write SetObjectClass;
    property OnCreateObject: TDatasetCreateObjectEvent read FOnCreateObject write FOnCreateObject;
    property OnObjectInsert: TDatasetObjectEvent read FOnObjectInsert write FOnObjectInsert;
    property OnObjectUpdate: TDatasetObjectEvent read FOnObjectUpdate write FOnObjectUpdate;
    property OnObjectRemove: TDatasetObjectEvent read FOnObjectRemove write FOnObjectRemove;
  end;

  TCustomAureliusDataset = class(TBaseAureliusDataset)
  private
    FManager: TObjectManager;
    procedure SetManager(const Value: TObjectManager);
  protected
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;
    procedure SetDataSetField(const Value: TDataSetField); override;
  protected
    function GetExplorer: TMappingExplorer; override;
    procedure InternalObjectRemove(Obj: TObject); override;
    procedure InternalObjectInsert(Obj: TObject); override;
    procedure InternalObjectUpdate(Obj: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Manager: TObjectManager read FManager write SetManager;
    property InternalList;
  end;

  TAureliusDataset = class(TCustomAureliusDataset)
  published
    property DatasetField;
    property FieldDefs;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnFilterRecord;

    property OnCreateObject;
    property OnObjectInsert;
    property OnObjectUpdate;
    property OnObjectRemove;
  end;

  TObjBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TBaseAureliusDataset;
    FBuffer: TAureliusRecordBuffer;
    FFieldNo: integer;
    FModified: boolean;
  protected
    procedure ReadBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

implementation

uses
  DBConsts, Variants, FmtBCD, Rtti,
  {$IFDEF DELPHIXE4_LVL}
  {$IFNDEF NEXTGEN}
  AnsiStrings,
  {$ENDIF}
  {$ENDIF}
  Aurelius.Bind.Exceptions,
  Aurelius.Engine.ObjectFactory,
  Aurelius.Global.Config,
  Aurelius.Global.Utils,
  Aurelius.Mapping.Metadata,
  Aurelius.Mapping.RttiUtils;

const
  SelfFieldName = 'Self';
  EnumNameSufix = 'EnumName';

{$IFNDEF DELPHIXE2_LVL}
type
  IntPtr = integer;
{$ENDIF}

type
  TInternalCriteria = class(TCriteria)
  end;

{ TBaseAureliusDataset }

{$IFDEF NEXTGEN}
function TBaseAureliusDataset.AllocRecBuf: TRecBuf;
begin
  Result := TRecBuf(AllocMem(FRecBufSize));
  Initialize(GetBufferValueList(Result)^, Fields.Count);
end;
{$ELSE}
function TBaseAureliusDataset.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
  {$IFDEF DELPHIXE4_LVL}
  Initialize(GetBufferValueList(NativeInt(Result))^, Fields.Count);
  {$ELSE}
  Initialize(GetBufferValueList(Result)^, Fields.Count);
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF NEXTGEN}
procedure TBaseAureliusDataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  Finalize(GetBufferValueList(Buffer)^, Fields.Count);
  FreeMem(Pointer(Buffer), FRecBufSize);
end;
{$ELSE}
procedure TBaseAureliusDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  {$IFDEF DELPHIXE4_LVL}
  Finalize(GetBufferValueList(NativeInt(Buffer))^, Fields.Count);
  {$ELSE}
  Finalize(GetBufferValueList(Buffer)^, Fields.Count);
  {$ENDIF}
  FreeMem(Buffer, FRecBufSize);
end;
{$ENDIF}

constructor TBaseAureliusDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalList := TList<TObject>.Create;
  FModifiedFields := TList<TField>.Create;
  BookmarkSize := SizeOf(TObject);
  ObjectView := True;
end;

function TBaseAureliusDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TObjBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TBaseAureliusDataset.CreateFields;
begin
  inherited;
end;

function TBaseAureliusDataset.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  Result := inherited CreateNestedDataset(DataSetField);
end;

function TBaseAureliusDataset.CreateObject: TObject;
begin
  Result := DoCreateObject;
  if Result = nil then
    Result := TObjectFactory.GetInstance.CreateInstance(FObjectClass);
end;

function TBaseAureliusDataset.Current<T>: T;
var
  RecBuf: TAureliusRecordBuffer;
begin
  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit(nil);
  Result := T(GetBufferRecInfo(RecBuf)^.Obj);
end;

{$IFDEF DELPHIXE2_LVL}
procedure TBaseAureliusDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TBaseAureliusDataset.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF}
begin
  case Event of
    deParentScroll:
      if DataSetField <> nil then
      begin
        UpdateListFromParent(DataSetField);
        Resync([]);
      end;
  end;
  inherited;
end;

destructor TBaseAureliusDataset.Destroy;
begin
  FModifiedFields.Free;
  FInternalList.Free;
  inherited;
end;

procedure TBaseAureliusDataset.DoAfterOpen;
var
  I: Integer;
begin
  // Notify nested datasets to be refreshed
  if Assigned(NestedDataSets) then
  begin
    for I := 0 to NestedDataSets.Count - 1 do
      if TObject(NestedDatasets[I]) is TBaseAureliusDataset then
        with TBaseAureliusDataset(NestedDataSets[I]) do
          if Active then
          begin
            UpdateListFromParent(DatasetField);
            Resync([]);
          end;
  end;
  inherited;
end;

function TBaseAureliusDataset.DoCreateObject: TObject;
begin
  Result := nil;
  if Assigned(FOnCreateObject) then
    FOnCreateObject(Self, Result);
end;

procedure TBaseAureliusDataset.DoObjectInsert(Obj: TObject);
begin
  if Assigned(FOnObjectInsert) then
    FOnObjectInsert(Self, Obj)
  else
    InternalObjectInsert(Obj);
end;

procedure TBaseAureliusDataset.DoObjectRemove(Obj: TObject);
begin
  if Assigned(FOnObjectRemove) then
    FOnObjectRemove(Self, Obj)
  else
   InternalObjectRemove(Obj);
end;

procedure TBaseAureliusDataset.DoObjectUpdate(Obj: TObject);
begin
  if Assigned(FOnObjectUpdate) then
    FOnObjectUpdate(Self, Obj)
  else
    InternalObjectUpdate(Obj);
end;

procedure TBaseAureliusDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;
  inherited;
end;

function TBaseAureliusDataset.EntityFieldByName(const FieldName: string): TAureliusEntityField;
var
  Field: TField;
begin
  Field := FindField(FieldName);
  if Field = nil then DatabaseErrorFmt(SFieldNotFound, [FieldName], Self);
  if not (Field is TAureliusEntityField) then
    DatabaseErrorFmt(SFieldTypeMismatch, [FieldName, 'TEntityField', Field.ClassName], Self);
  Result := TAureliusEntityField(Field);
end;

procedure TBaseAureliusDataset.FetchAllRecords;
begin
  while FetchMoreRecords do ;  
end;

function TBaseAureliusDataset.FetchingRecords: boolean;
begin
  Result := (FSourceCursor <> nil) or (FSourceCriteria <> nil);
end;

function TBaseAureliusDataset.FetchMoreRecords: boolean;
var
  LocalCursor: ICriteriaCursor;
begin
  if not FetchingRecords then Exit(false);

  Result := false;
  if FSourceCursor <> nil then
  begin
    // Fetch using cursor
    Result := FSourceCursor.Next;
    if Result then
      FSourceList.Add(FSourceCursor.Fetch)
    else
      FSourceCursor := nil;
  end
  else
  if FSourceCriteria <> nil then
  begin
    // Fetch using paging
    FSourceCriteria.Skip(FCurrentFirstRow);
    if FPageSize > 0 then // if pagesize not specified, then retrieve all records
    begin
      if FPageSize > FRemainingRows then
        FSourceCriteria.Take(FRemainingRows)
      else
        FSourceCriteria.Take(FPageSize);
    end;

    LocalCursor := TInternalCriteria(FSourceCriteria).OpenAndKeep;
    while LocalCursor.Next do
    begin
      FSourceList.Add(LocalCursor.Fetch);
      Result := True;
      Dec(FRemainingRows);
      Inc(FCurrentFirstRow);
    end;

    // Whenever we don't need more records, destroy the source criteria so we disable fetching records
    if (not Result) or (FPageSize <= 0) or (FRemainingRows = 0) then
    begin
      FSourceCriteria.Free;
      FSourceCriteria := nil;
    end;
  end;
end;

function TBaseAureliusDataset.FindPropInfoByName(Clazz: TClass;
  APropName: string): TRttiOptimization;
var
  O: TRttiOptimization;
begin
  for O in Explorer.GetClassVisibleMembers(Clazz, False) do
    if SameText(O.MemberName, APropName) then
      Exit(O);
  Result := nil;
end;

function TBaseAureliusDataset.GetActiveRecBuf: TAureliusRecordBuffer;
begin
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        Result := NullBuffer
      else
        Result := ActiveBuffer;
    dsCalcFields, dsInternalCalc:
      Result := CalcBuffer;
    dsFilter:
      Result := FFilterBuffer;
    dsOldValue:
      if FOldValueBuffer <> NullBuffer then
        Result := FOldValueBuffer
      else
        Result := ActiveBuffer;
    dsEdit, dsInsert, dsNewValue:
      Result := ActiveBuffer;
  else
    Result := NullBuffer;
  end;
end;

function TBaseAureliusDataset.GetBlobData(Field: TField): TBytes;
var
  Data: ValueType;
begin
  if GetFieldVariant(Field, Data) then
  begin
    // if field is special cases memo or widememo, and variant is string
    // then force the encoding for the expected encoding.
    // Memo needs ANSI encoding, WideMemo needs Unicode encoding.
    if (Field.DataType in [ftMemo, ftWideMemo]) and VarIsStr(Data) then
    begin
      case Field.DataType of
        ftMemo:
          {$IFDEF DELPHIXE2_LVL}
          Result := TEncoding.ANSI.GetBytes(VarToStr(Data));
          {$ELSE}
          Result := TEncoding.Default.GetBytes(VarToStr(Data));
          {$ENDIF}
        ftWideMemo:
          Result := TEncoding.Unicode.GetBytes(VarToStr(Data));
      end;
    end
    else
      Result := TUtils.VariantToBytes(Data);
  end
  else
    SetLength(Result, 0);
end;

procedure TBaseAureliusDataset.GetBookmarkData(Buffer: TAureliusRecordBuffer; Data: TAureliusBookmark);
begin
  PObject(Data)^ := GetBufferRecInfo(Buffer)^.Obj;
end;

function TBaseAureliusDataset.GetBookmarkFlag(Buffer: TAureliusRecordBuffer): TBookmarkFlag;
begin
  Result := GetBufferRecInfo(Buffer)^.BookmarkFlag;
end;

function TBaseAureliusDataset.GetExplorer: TMappingExplorer;
begin
  Result := TMappingExplorer.DefaultInstance;
end;

function TBaseAureliusDataset.GetBufferRecInfo(Buffer: TAureliusRecordBuffer): PRecInfo;
begin
  Result := PRecInfo(Buffer + FRecInfoOffset);
end;

function TBaseAureliusDataset.GetBufferValueList(Buffer: TAureliusRecordBuffer): PValueList;
begin
  Result := PValueList(Buffer);
end;

function TBaseAureliusDataset.GetCanModify: boolean;
begin
  Result := True;
end;

function TBaseAureliusDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  case FieldType of
    ftVariant: Result := TAureliusEntityField;
  else
    result := inherited GetFieldClass(FieldType);
  end;
end;

{$IFDEF DELPHIXE4_LVL}
function TBaseAureliusDataset.GetFieldData(Field: TField; var Buffer: TAureliusValueBuffer): boolean;
{$ELSE}
function TBaseAureliusDataset.GetFieldData(Field: TField; Buffer: TAureliusValueBuffer): boolean;
{$ENDIF}
var
  Data: ValueType;

  procedure VariantToFieldBuffer;
  var
    Str: string;
    SInt: Smallint;
    I: integer;
    I64: LargeInt;
    B: boolean;
    F: double;
    BCD: TBCD;
    D: TDateTime;
    {$IFDEF DELPHIXE3_LVL}
    TempValue: TValueBuffer;
    {$ENDIF}
    {$IFDEF NEXTGEN}
    M: TMarshaller;
    {$ENDIF}
  begin
    case Field.DataType of
      ftString, ftFixedChar:
        begin
        {$IFNDEF NEXTGEN}
          {$IFDEF DELPHIXE3_LVL}
          FillChar(Buffer[0], Field.DataSize, 0);
          {$ELSE}
          FillChar(Buffer^, Field.DataSize, 0);
          {$ENDIF}
          Str := VarToStr(Data);
          {$IFDEF DELPHIXE4_LVL}
          AnsiStrings.StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
          {$ELSE}
          StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
          {$ENDIF}
        {$ELSE}
          FillChar(Buffer[0], Field.DataSize, 0);
          Str := VarToStr(Data);
          TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
        {$ENDIF}
        end;
      ftWideString, ftFixedWideChar:
        begin
          {$IFDEF DELPHIXE3_LVL}
          FillChar(Buffer[0], Field.DataSize, 0);
          {$ELSE}
          FillChar(Buffer^, Field.DataSize, 0);
          {$ENDIF}
          Str := VarToStr(Data);
          StrLCopy(PChar(Buffer), PChar(Str), Field.Size);
        end;
      ftSmallint:
        begin
          SInt := Data;
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromSmallInt(SInt, Buffer);
          {$ELSE}
          Smallint(Buffer^) := SInt;
          {$ENDIF}
        end;
      ftInteger:
        begin
          I := Data;
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromInteger(I, Buffer);
          {$ELSE}
          integer(Buffer^) := I;
          {$ENDIF}
        end;
      ftLargeint:
        begin
          I64 := Data;
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromLargeInt(I64, Buffer);
          {$ELSE}
          LargeInt(Buffer^) := I64;
          {$ENDIF}
        end;
      ftBoolean:
        begin
          B := Data;
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromWordBool(B, Buffer);
          {$ELSE}
          WordBool(Buffer^) := B;
          {$ENDIF}
        end;
      ftFloat, ftCurrency:
        begin
          F := Data;
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromDouble(F, Buffer);
          {$ELSE}
          double(Buffer^) := F;
          {$ENDIF}
        end;
      ftFmtBCD:
        begin
          BCD := VarToBcd(Data);
          {$IFDEF DELPHIXE3_LVL}
          TBitConverter.FromBcd(BCD, Buffer);
          {$ELSE}
          TBCD(Buffer^) := BCD;
          {$ENDIF}
        end;
      ftDate, ftTime, ftDateTime:
        begin
          D := VarToDateTime(Data);
          {$IFDEF DELPHIXE3_LVL}
          if True then
          begin
            SetLength(TempValue, SizeOf(double));
            TBitConverter.FromDouble(D, TempValue);
            DataConvert(Field, TempValue, Buffer, True);
          end
          else
            TBitConverter.FromDouble(D, Buffer);
          {$ELSE}
          //if NativeFormat then
          if True then
            DataConvert(Field, @D, Buffer, True)
          else
            TDateTime(Buffer^) := D;
          {$ENDIF}
        end;
      ftBlob, ftMemo, ftWideMemo, ftGraphic:
        begin
           // Not needed, blob stream uses GetBlobData directly
//          TempBytes := TUtils.VariantToBytes(Data);
//          System.Move(TempBytes[0], Buffer^, Length(TempBytes));
//          Variant(Buffer^) := Data;
        end;
      ftVariant:
        begin
          {$IFDEF DELPHIXE3_LVL}
          Variant(PVariant(@Buffer[0])^) := Data;
          {$ELSE}
          Variant(Buffer^) := Data;
          {$ENDIF}
        end;
      ftGuid:
        begin
        {$IFNDEF NEXTGEN}
          // Same as string
          {$IFDEF DELPHIXE3_LVL}
          FillChar(Buffer[0], Field.DataSize, 0);
          {$ELSE}
          FillChar(Buffer^, Field.DataSize, 0);
          {$ENDIF}
          Str := VarToStr(Data);
          {$IFDEF DELPHIXE4_LVL}
          AnsiStrings.StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
          {$ELSE}
          StrLCopy(PAnsiChar(Buffer), PAnsiChar(AnsiString(Str)), Field.Size);
          {$ENDIF}
        {$ELSE}
          // Same as string
          FillChar(Buffer[0], Field.DataSize * SizeOf(Char), 0);
          Str := VarToStr(Data);
          TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
        {$ENDIF}
        end
    else

      // ftWord,ftBytes,ftVarBytes,ftAutoInc,ftGraphic,ftFmtMemo,ftParadoxOle,ftDBaseOle,ftTypedBinary,
      // ftCursor,ftADT,ftArray,ftReference,ftDataSet,ftOraBlob,ftOraClob,ftVariant,ftInterface,ftIDispatch,
      // ftGuid,ftTimeStamp,ftFMTBcd,ftOraTimeStamp,ftOraInterval,ftLongWord,ftShortint,ftByte,ftExtended,
      // ftConnection,ftParams,ftStream,ftTimeStampOffset,ftObject,ftSingle,
    end;
  end;

  procedure ValueToFieldBuffer;
  begin
    VariantToFieldBuffer;
  end;

begin
  Result := GetFieldVariant(Field, Data);
  {$IFDEF DELPHIXE3_LVL}
  if Length(Buffer) = 0 then Exit;
  {$ENDIF}
  if Result and (Buffer <> nil) then
    ValueToFieldBuffer;
end;

function TBaseAureliusDataset.GetFieldVariant(Field: TField;
  var Data: ValueType): boolean;
var
  RecBuf: TAureliusRecordBuffer;
  Obj: TObject;
begin
  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit(false);
  Data := GetBufferValueList(RecBuf)[Field.Index];
  if VarIsEmpty(Data) then
  begin
    Obj := GetBufferRecInfo(RecBuf)^.Obj;
    if IsSelfField(Field) then
      Data := ObjectToVariant(Obj)
    else
    begin
      if Field.FieldKind = fkData then
        Data := GetPropValue(Field.FieldName, Obj);
    end;
    if VarIsEmpty(Data) then
      Data := Null;
    GetBufferValueList(RecBuf)[Field.Index] := Data;
  end;
  Result := not VarIsNull(Data);
end;

function TBaseAureliusDataset.GetInternalList: IReadOnlyObjectList;
begin
  Result := Explorer.AsList(FInternalList);
end;

function TBaseAureliusDataset.GetPropValue(APropName: string; Obj: TObject): ValueType;
var
  Value: TValue;
  O: TRttiOptimization;
  Prop, SubProp: string;
  CriteriaResult: TCriteriaResult;
begin
  if Obj = nil then Exit(Null);
  SplitProp(APropName, Prop, SubProp);

  // Special treatment for TCriteriaResult objects
  if Obj is TCriteriaResult then
  begin
    CriteriaResult := TCriteriaResult(Obj);
    if CriteriaResult.HasProp(Prop) and (SubProp = '') then
      Exit(CriteriaResult.Values[Prop])
    else
      Exit(Null);
  end;

  O := FindPropInfoByName(Obj.ClassType, Prop);
  if O = nil then Exit(Null);

  Value := Explorer.GetMemberValue(Obj, O);
  if O.RealType.IsInstance then
  begin
    if SubProp = '' then
      Result := ObjectToVariant(Value.AsObject)
    else
      Result := GetPropValue(SubProp, Value.AsObject);
  end
  else
  begin
    if SubProp = '' then
    begin
      if IsEnumeration(Value.TypeInfo) then
        Result := Value.AsOrdinal
      else
        Result := Explorer.ValueToVariant(Value, O)
    end
    else
      if IsEnumeration(Value.TypeInfo) and SameText(SubProp, EnumNameSufix) then
        Result := GetEnumName(Value.TypeInfo, Value.AsOrdinal)
      else
        Result := Null;
  end;
end;

function TBaseAureliusDataset.GetRecNo: Longint;
var
  RecBuf: TAureliusRecordBuffer;
begin
  CheckActive;
  Result := -1;
  RecBuf := GetActiveRecBuf;
  if (RecBuf <> NullBuffer) and (GetBufferRecInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := GetBufferRecInfo(RecBuf)^.Index + 1;
end;

function TBaseAureliusDataset.InternalGetRecord(Buffer: TAureliusRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  try
    Result := grOK;
    case GetMode of
      gmNext:
        Inc(FCurrent);
      gmPrior:
        Dec(FCurrent);
    end;
    if FCurrent < 0 then
    begin
      Result := grBOF;
      FCurrent := -1;
    end
    else if FCurrent >= ListCount then
    begin
      if not FetchMoreRecords then
      begin
        Result := grEOF;
        if ListCount = 0 then
          FCurrent := -1
        else
          FCurrent := ListCount;
      end;
    end;

    if Result = grOK then
    begin
      with GetBufferRecInfo(Buffer)^ do
      begin
        Index := FCurrent;
        Obj := FSourceList.Item(FCurrent);
        BookmarkFlag := bfCurrent;
      end;
      Finalize(GetBufferValueList(Buffer)^, Fields.Count);
      GetCalcFields(Buffer);
    end;
  except
    Result := grError;
    if DoCheck then
      raise ;
  end;
end;

function TBaseAureliusDataset.GetRecord(Buffer: TAureliusRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
var
  SaveState: TDataSetState;
  AcceptRecord: Boolean;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState := SetTempState(dsFilter);
    try
      AcceptRecord := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, AcceptRecord);
          if not AcceptRecord and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until AcceptRecord or (Result <> grOK);
    except
      InternalHandleException;
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck)
end;

function TBaseAureliusDataset.GetRecordCount: integer;
begin
  if FetchingRecords then
    Result := -1
  else
    Result := ListCount;
end;

function TBaseAureliusDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

{$IFDEF DELPHIXE3_LVL}
procedure TBaseAureliusDataset.GetTheFieldList(List: TList<TField>;
  const FieldNames: string);
begin
  GetFieldList(List, FieldNames);
end;
{$ELSE}
procedure TBaseAureliusDataset.GetTheFieldList(List: TList<TField>;
  const FieldNames: string);
var
  TheFields: TList;
  I: integer;
begin
  TheFields := TList.Create;
  try
    GetFieldList(TheFields, FieldNames);
    List.Clear;
    for I := 0 to TheFields.Count - 1 do
      List.Add(TField(TheFields[I]));
  finally
    TheFields.Free;
  end;
end;
{$ENDIF}

procedure TBaseAureliusDataset.InitFieldDefsFromClass(AClass: TClass);
var
  O: TRttiOptimization;
  DataType: TFieldType;
  C: TColumn;
  A: TAssociation;
  Len: integer;
  FieldDef: TFieldDef;
begin
  FieldDefs.Clear;
  if AClass = nil then
    Exit;
  if not (csDesigning in ComponentState) and (FieldCount > 0) then
  begin
    InitFieldDefsFromFields;
    Exit;
  end;

  if True then // Add Self field def? True by default
  begin
    FieldDef := FieldDefs.AddFieldDef;
    FieldDef.Name := SelfFieldName;
    FieldDef.DataType := ftVariant;
    FieldDef.Attributes := FieldDef.Attributes + [TFieldAttribute.faReadonly];
  end;

  for O in Explorer.GetClassVisibleMembers(AClass, True) do
  begin
    if O.RealType.IsInstance then
    begin
      // Associations
      A := Explorer.FindAssociationByPropertyName(AClass, O.MemberName);
      if A = nil then
        Continue;

      if A.Kind = TAssociationKind.SingleValued then
      begin
        FieldDef := FieldDefs.AddFieldDef;
        FieldDef.Name := O.MemberName;
        FieldDef.DataType := ftVariant;
        FieldDef.Required := A.Required;

        if (O.MemberRef is TRttiProperty) and not TRttiProperty(O.MemberRef).IsWritable then
          FieldDef.Attributes := FieldDef.Attributes + [TFieldAttribute.faReadonly];
      end else
      begin
        FieldDef := FieldDefs.AddFieldDef;
        FieldDef.Name := O.MemberName;
        FieldDef.DataType := ftDataSet;
        FieldDef.Required := false;
        FieldDef.Attributes := FieldDef.Attributes + [TFieldAttribute.faReadonly];
      end;
    end
    else
    begin
      // Assume one column for each member, for scalar (non-object) types.
      // If this ever changes (multiple columns per scalar property) review this
      C := Explorer.FindColumnByPropertyName(AClass, O.MemberName);
      if C <> nil then
        Len := C.Length
      else
        Len := 0;
      DataType := Explorer.RttiTypeToFieldType(O.RealType, Len);

      // Len is used to determine string or memo
      if (Len = 0) and (DataType in [ftString, ftWideString]) then
        Len := TGlobalConfigs.GetInstance.DefaultStringColWidth;

      // Force guid fields to be size of 38 (required by Guid fields)
      if DataType = ftGuid then
        Len := 38;

      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := O.MemberName;
      FieldDef.DataType := DataType;

      // Only set length for field types that need size. This avoids problems, for example, with enumerated types
      // enumerated field type will always be integer here (due to call to RttiTypeToFieldType above. But if the enumerated
      // is mapped in model as a string field, then Len will be not zero here (although datatype is ftInteger).
      // So we must keep Size 0 for integer fields otherwise an error will be raised by the dataset
      if not (FieldDef.DataType in ftFixedSizeTypes) then
        FieldDef.Size := Len;

      if C <> nil then
        FieldDef.Required := TColumnProp.Required in C.Properties
      else
        FieldDef.Required := not (O.IsNullable or (DataType in [ftMemo, ftBlob, ftWideMemo]));

      // Make read-only properties to become read-only fields. Except dynamic properties which are always writeable
      if not O.IsDynamic and (O.MemberRef is TRttiProperty) and not TRttiProperty(O.MemberRef).IsWritable then
        FieldDef.Attributes := FieldDef.Attributes + [TFieldAttribute.faReadonly];

      if (C <> nil) and (TColumnProp.NoUpdate in C.Properties) then
        FieldDef.Attributes := FieldDef.Attributes + [TFieldAttribute.faReadonly];
    end;
  end;
end;

//procedure TBaseAureliusDataset.InternalAddRecord(Buffer: Pointer; Append: boolean);
//begin
//end;

procedure TBaseAureliusDataset.InternalClose;
begin
  FIsOpen := false;
  FSourceCursor := nil;
  if FSourceCriteria <> nil then
  begin
    FSourceCriteria.Free;
    FSourceCriteria := nil;
  end;
  BindFields(false);
  if DefaultFields then
    DestroyFields;

  // Free internal buffers

  FCurrent := -1;
end;

procedure TBaseAureliusDataset.InternalDelete;
var
  RecBuf: TAureliusRecordBuffer;
  Obj: TObject;
  Index: integer;
begin
  RecBuf := GetActiveRecBuf;
  Obj := GetBufferRecInfo(RecBuf)^.Obj;
  Index := FSourceList.IndexOf(Obj);

  if Index > -1 then
  begin
    FSourceList.Delete(Index);
    if FCurrent >= ListCount then
      FCurrent := ListCount - 1;
    DoObjectRemove(Obj);
  end;
end;

procedure TBaseAureliusDataset.InternalEdit;
begin
  FModifiedFields.Clear;
  inherited;
end;

procedure TBaseAureliusDataset.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TBaseAureliusDataset.InternalGotoBookmark(Bookmark: TAureliusBookmark);
var
  Index: integer;
begin
  Index := FSourceList.IndexOf(PObject(Bookmark)^);
  if Index <> -1 then
    FCurrent := Index
  else
    DatabaseError('Bookmark not found');
end;

procedure TBaseAureliusDataset.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    ApplicationHandleException(Self);
end;

procedure TBaseAureliusDataset.InternalInitFieldDefs;
begin
  InitFieldDefsFromClass(FObjectClass);
end;

procedure TBaseAureliusDataset.InternalInitRecord(Buffer: TAureliusRecordBuffer);
var
  I: integer;
begin
  for I := 0 to Fields.Count - 1 do
    GetBufferValueList(Buffer)[I] := Null;
end;

{$IFDEF DELPHIXE4_LVL}
{$IFNDEF NEXTGEN}
procedure TBaseAureliusDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  InternalInitRecord(TAureliusRecordBuffer(Buffer));
end;
{$ENDIF}
{$ENDIF}

procedure TBaseAureliusDataset.InternalLast;
begin
  FetchAllRecords;
  FCurrent := ListCount;
end;

procedure TBaseAureliusDataset.InternalObjectInsert(Obj: TObject);
begin
end;

procedure TBaseAureliusDataset.InternalObjectRemove(Obj: TObject);
begin
end;

procedure TBaseAureliusDataset.InternalObjectUpdate(Obj: TObject);
begin
end;

procedure TBaseAureliusDataset.InternalOpen;
begin
  if DataSetField <> nil then
    UpdateListFromParent(DataSetField);
  if FSourceList = nil then
    raise ESourceListNotAssigned.Create;
  if FObjectClass = nil then
    raise EObjectClassNotSpecified.Create;
  FCurrent := -1;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);

  // Initialize buffer pointers
  FRecordSize := Fields.Count * SizeOf(ValueType);
  FRecBufSize := FRecordSize + SizeOf(TRecInfo);
  FRecInfoOffset := FRecordSize;

  // create internal buffers
  FIsOpen := True;
end;

procedure TBaseAureliusDataset.InternalPost;
var
  RecBuf: TAureliusRecordBuffer;

  procedure UpdateObjectFields(Obj: TObject);
  var
    Field: TField;
    Value: ValueType;
  begin
    for Field in FModifiedFields do
    begin
      if Field.FieldKind = fkData then
      begin
        Value := GetBufferValueList(RecBuf)[Field.Index];
        SetPropValue(Field.FieldName, Obj, Value);
      end;
    end;
  end;

var
  Obj: TObject;
  IndexToInsert: integer;
begin
  inherited;
  UpdateCursorPos;
  RecBuf := GetActiveRecBuf;
  if State = dsEdit then
  begin
    Obj := GetBufferRecInfo(RecBuf)^.Obj;
    UpdateObjectFields(Obj);
    DoObjectUpdate(Obj);
  end
  else
  begin
    case GetBufferRecInfo(RecBuf)^.BookmarkFlag of
      bfBOF:
        IndexToInsert := 0;
      bfEOF:
        IndexToInsert := -1;
    else
      IndexToInsert := FSourceList.IndexOf(GetBufferRecInfo(RecBuf)^.Obj);
    end;
    Obj := CreateObject;
    UpdateObjectFields(Obj);
    if IndexToInsert = -1 then
    begin
      FetchAllRecords;
      FSourceList.Add(Obj);
    end
    else
      FSourceList.Insert(IndexToInsert, Obj);
    DoObjectInsert(Obj);
  end;
end;

procedure TBaseAureliusDataset.InternalSetSourceList(SourceList: TObject);
begin
  if SourceList = nil then
  begin
    FSourceList := nil;
  end
  else
  begin
    if not Explorer.IsList(SourceList.ClassType) then
      raise EInvalidListObject.Create(SourceList);
    FSourceList := Explorer.AsList(SourceList);
    ObjectClass := TRttiUtils.GetInstance.GetSurroundedClass(SourceList.ClassType);
  end;
end;

{$IFDEF DELPHIXE3_LVL}
procedure TBaseAureliusDataset.InternalSetToRecord(Buffer: TAureliusRecordBuffer);
var
  BookmarkBuffer: TBookmark;
begin
  SetLength(BookmarkBuffer, BookmarkSize);
  PObject(BookmarkBuffer)^ := GetBufferRecInfo(Buffer)^.Obj;
  InternalGotoBookmark(BookmarkBuffer);
end;
{$ELSE}
procedure TBaseAureliusDataset.InternalSetToRecord(Buffer: TAureliusRecordBuffer);
begin
  InternalGotoBookmark(@GetBufferRecInfo(Buffer).Obj);
end;
{$ENDIF}

function TBaseAureliusDataset.IsCursorOpen: boolean;
begin
  Result := FIsOpen;
end;

function TBaseAureliusDataset.IsEnumeration(ATypeInfo: PTypeInfo): boolean;
begin
  Result:= (ATypeInfo <> nil) and (ATypeInfo.Kind = tkEnumeration) and (ATypeInfo <> TypeInfo(boolean));
end;

function TBaseAureliusDataset.IsSelfField(Field: TField): boolean;
begin
  Result := Field.FieldName = SelfFieldName;
end;

function TBaseAureliusDataset.IsSequenced: Boolean;
begin
  Result := not FetchingRecords;
end;

function TBaseAureliusDataset.ListCount: integer;
begin
  Result := FSourceList.Count;
end;

function TBaseAureliusDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True, '');
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TBaseAureliusDataset.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean; ResultFields: string): Boolean;
var
  FieldList: TList<TField>;

  function MatchField(Field: TField; Value: Variant; Options: TLocateOptions): Boolean;
  var
    FieldValue: string;
  begin
    case Field.DataType of
      ftString, ftFixedChar, ftWideString, ftFixedWideChar:
        begin
          FieldValue := VarToStr(Field.Value);
          if loPartialKey in Options then
            FieldValue := Copy(FieldValue, 1, Length(Value));
          if loCaseInsensitive in Options then
            Result := SameText(VarToStr(Value), FieldValue)
          else
            Result := SameStr(VarToStr(Value), FieldValue);
        end;
    else
      Result := (Field.Value = Value);
    end;
  end;

  function MatchRecord: boolean;
  var
    I: integer;
  begin
    Result := False;
    for I := 0 to FieldList.Count - 1 do
    begin
      if FieldList.Count = 1 then
        Result := MatchField(FieldList[I], KeyValues, Options)
      else
        Result := MatchField(FieldList[I], KeyValues[I], Options);
      if not Result then
        Break;
    end;
  end;

var
  OldIndex: Integer;
  Buffer: TAureliusRecordBuffer;
  ResultFieldList: TList<TField>;
  I: integer;
  {$IFDEF DELPHIXE4_LVL}
  NullValueBuffer: TValueBuffer;
  {$ENDIF}
begin
  CheckBrowseMode;
  CursorPosChanged;
  Result := False;
  FieldList := TList<TField>.Create;
  try
    GetTheFieldList(FieldList, KeyFields);
    OldIndex := FCurrent;
    SetTempState(dsFilter);
    Buffer := TempBuffer;
    FFilterBuffer := Buffer;
    try
      InternalFirst;
      while GetRecord(Buffer, gmNext, True) = grOK do
        if MatchRecord then
        begin
          Result := True;
          break;
        end;


      if Result and not SyncCursor then
      begin
        // For lookups, read needed field values into the temp buffer
        ResultFieldList := TList<TField>.Create;
        try
          GetTheFieldList(ResultFieldList, ResultFields);
          // For optimization, update only the values for desired result fields
          for I := 0 to ResultFieldList.Count - 1 do
          begin
            {$IFDEF DELPHIXE4_LVL}
              SetLength(NullValueBuffer, 0);
              GetFieldData(ResultFieldList[I], NullValueBuffer);
            {$ELSE}
              {$IFDEF DELPHIXE3_LVL}
              GetFieldData(ResultFieldList[I], TValueBuffer(nil));
              {$ELSE}
              GetFieldData(ResultFieldList[I], nil);
              {$ENDIF}
            {$ENDIF}
          end;
//          CalculateFields(Buffer);
        finally
          ResultFieldList.Free;
        end;
      end;

      // Restore old cursor position
      if not (Result and SyncCursor) then
        FCurrent := OldIndex;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    FieldList.Free;
  end;
end;

function TBaseAureliusDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  VarClear(Result);
  if LocateRecord(KeyFields, KeyValues, [], False, ResultFields) then
  begin
    SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

class function TBaseAureliusDataset.ObjectToVariant(Obj: TObject): Variant;
begin
  {$IFDEF DELPHIXE2_LVL}
  Result := IntPtr(Obj);
  {$ELSE}
  Result := integer(Obj);
  {$ENDIF}
end;

procedure TBaseAureliusDataset.SetBookmarkData(Buffer: TAureliusRecordBuffer; Data: TAureliusBookmark);
begin
  GetBufferRecInfo(Buffer)^.Obj := PObject(Data)^;
end;

procedure TBaseAureliusDataset.SetBookmarkFlag(Buffer: TAureliusRecordBuffer; Value: TBookmarkFlag);
begin
  GetBufferRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TBaseAureliusDataset.SetDataSetField(const Value: TDataSetField);
begin
  if Assigned(Value) then
    UpdateListFromParent(Value);
  inherited SetDataSetField(Value);
end;

procedure TBaseAureliusDataset.SetFieldData(Field: TField; Buffer: TAureliusValueBuffer);
begin
  SetFieldData(Field, Buffer, True);
end;

procedure TBaseAureliusDataset.SetFieldData(Field: TField; Buffer: TAureliusValueBuffer; NativeFormat: boolean);

  function FieldBufferToVariant(Buffer: TAureliusValueBuffer): Variant;
  var
    SInt: Smallint;
    I: integer;
    I64: LargeInt;
    B: boolean;
    F: double;
    BCD: TBCD;
    {$IFDEF DELPHIXE3_LVL}
    TempValue: TValueBuffer;
    {$ELSE}
    D: TDateTime;
    {$ENDIF}
    {$IFDEF NEXTGEN}
    NullIndex: integer;
    Str: string;
    {$ENDIF}
  begin
    case Field.DataType of
      ftString, ftFixedChar:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(PAnsiChar(Buffer));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
      ftWideString, ftFixedWideChar:
        begin
          {$IFNDEF NEXTGEN}
          Result := WideString(PWideChar(Buffer));
          {$ELSE}
          Result := string(PChar(Buffer));
          {$ENDIF}
        end;
      ftSmallint:
        begin
          {$IFDEF DELPHIXE3_LVL}
          SInt := TBitConverter.ToSmallInt(Buffer);
          {$ELSE}
          SInt := Smallint(Buffer^);
          {$ENDIF}
          Result := SInt;
        end;
      ftInteger:
        begin
          {$IFDEF DELPHIXE3_LVL}
          I := TBitConverter.ToInteger(Buffer);
          {$ELSE}
          I := integer(Buffer^);
          {$ENDIF}
          Result := I;
        end;
      ftLargeint:
        begin
          {$IFDEF DELPHIXE3_LVL}
          I64 := TBitConverter.ToLargeInt(Buffer);
          {$ELSE}
          I64 := LargeInt(Buffer^);
          {$ENDIF}
          Result := I64;
        end;
      ftBoolean:
        begin
          {$IFDEF DELPHIXE3_LVL}
          B := TBitConverter.ToWordBool(Buffer);
          {$ELSE}
          B := WordBool(Buffer^);
          {$ENDIF}
          Result := B;
        end;
      ftFloat, ftCurrency:
        begin
          {$IFDEF DELPHIXE3_LVL}
          F := TBitConverter.ToDouble(Buffer);
          {$ELSE}
          F := double(Buffer^);
          {$ENDIF}
          Result := F;
        end;
      ftFmtBCD:
        begin
          {$IFDEF DELPHIXE3_LVL}
          BCD := TBitConverter.ToBcd(Buffer);
          {$ELSE}
          BCD := TBCD(Buffer^);
          {$ENDIF}
          VarFMTBcdCreate(Result, BCD);
        end;
      ftDate, ftTime, ftDateTime:
        begin
          {$IFDEF DELPHIXE3_LVL}
          if NativeFormat then
          begin
            SetLength(TempValue, SizeOf(TVarData(Result).VDate));
            DataConvert(Field, Buffer, TempValue, False);
            TVarData(Result).VDate := TBitConverter.ToDouble(TempValue);
          end else
            Result := TDateTime(TBitConverter.ToDouble(Buffer));
          {$ELSE}
          if NativeFormat then
          begin
            DataConvert(Field, Buffer, @D, False);
            Result := D;
          end
          else
            Result := TDateTime(Buffer^);
          {$ENDIF}
        end;
      ftBlob, ftGraphic:
        begin
          Result := TUtils.BytesToVariant(TBytes(Buffer));
        end;
      ftMemo:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(StringOf(TBytes(Buffer)));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
      ftWideMemo:
        begin
          Result := WideStringOf(TBytes(Buffer));
        end;
      ftVariant:
        begin
          {$IFDEF DELPHIXE3_LVL}
          Result := Variant(PVariant(@Buffer[0])^);
          {$ELSE}
          Result := Variant(Buffer^);
          {$ENDIF}
        end;
      ftGuid:
        begin
          {$IFNDEF NEXTGEN}
          Result := AnsiString(PAnsiChar(Buffer));
          {$ELSE}
          Str := TEncoding.ANSI.GetString(Buffer);
          NullIndex := Str.IndexOf(#0);
          if NullIndex >= 0 then
            Result := Str.Remove(NullIndex)
          else
            Result := Str;
          {$ENDIF}
        end;
    end;
  end;

  function FieldBufferToValue(Buffer: TAureliusValueBuffer): Variant;
  begin
    Result := FieldBufferToVariant(Buffer);
  end;

var
  RecBuf: TAureliusRecordBuffer;
  Data: ValueType;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);
  if IsSelfField(Field) then
    Exit;

  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit;

  if Field.ReadOnly and not(State in [dsSetKey, dsFilter]) then
    DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  Field.Validate(Buffer);
  if FModifiedFields.IndexOf(Field) = -1 then
    FModifiedFields.Add(Field);

  if Buffer = nil then
    Data := Null
  else
    Data := FieldBufferToValue(Buffer);
  GetBufferValueList(RecBuf)[Field.Index] := Data;
  if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, IntPtr(Field));
end;

procedure TBaseAureliusDataset.SetFiltered(Value: Boolean);
begin
  if Value <> Filtered then
  begin
    inherited;
    if Active then
    begin
      if Filtered then
        First
      else
        Refresh;
    end;
  end;
end;

procedure TBaseAureliusDataset.SetObjectClass(const Value: TClass);
begin
  if FObjectClass <> Value then
  begin
    FObjectClass := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TBaseAureliusDataset.SetPropValue(APropName: string; Obj: TObject; Value: ValueType);
var
  MemberValue: TValue;
  O: TRttiOptimization;
  Prop, SubProp: string;
begin
  if Obj = nil then
    raise ECannotSetProperty.Create(APropName);
  SplitProp(APropName, Prop, SubProp);

  O := FindPropInfoByName(Obj.ClassType, Prop);
  if O = nil then
    raise ECannotSetProperty.Create(APropName);

  if SubProp = '' then
  begin
    if O.RealType.IsInstance then
      MemberValue := TValue.From<TObject>(VariantToObject(Value))
    else
    begin
      if IsEnumeration(O.RealType.Handle) then
        MemberValue := TValue.FromOrdinal(O.RealType.Handle, Value)
      else
        MemberValue := Explorer.VariantToValue(Value, O);
    end;
    Explorer.SetMemberValue(Obj, O, MemberValue);
  end else
  begin
    if O.RealType.IsInstance then
    begin
      MemberValue := Explorer.GetMemberValue(Obj, O);
      SetPropValue(SubProp, MemberValue.AsObject, Value);
    end else
    begin
      if IsEnumeration(O.RealType.Handle) and SameText(SubProp, EnumNameSufix) then
      begin
        MemberValue := TValue.FromOrdinal(O.RealType.Handle, GetEnumValue(O.RealType.Handle, Value));
        Explorer.SetMemberValue(Obj, O, MemberValue);
      end
      else
        raise ECannotSetProperty.Create(APropName);
    end;
  end;
end;

procedure TBaseAureliusDataset.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  if Value < 1 then
    Value := 1;
  if Value > ListCount then
  begin
    while FetchingRecords and (Value > ListCount) do
      FetchMoreRecords;
    if Value > ListCount then
      Value := ListCount;
  end;
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TBaseAureliusDataset.SetSourceCriteria(Criteria: TCriteria; PageSize: integer);
begin
  CheckInactive;
  FSourceCriteria := Criteria;
  FRemainingRows := TInternalCriteria(FSourceCriteria).MaxRows;
  if FRemainingRows <= 0 then
    FRemainingRows := MaxInt;
  FCurrentFirstRow := TInternalCriteria(FSourceCriteria).FirstRow;
  FPageSize := PageSize;

  FInternalList.Clear;
  InternalSetSourceList(FInternalList);

  ObjectClass := FSourceCriteria.Clazz;
end;

procedure TBaseAureliusDataset.SetSourceCriteria(Criteria: TCriteria);
begin
  SetSourceCriteria(Criteria, 0);
end;

procedure TBaseAureliusDataset.SetSourceCursor(Cursor: ICriteriaCursor);
begin
  CheckInactive;
  FInternalList.Clear;
  FSourceCursor := Cursor;
  InternalSetSourceList(FInternalList);
  ObjectClass := Cursor.BaseClass;
end;

procedure TBaseAureliusDataset.SetSourceList(SourceList: TObject);
begin
  CheckInactive;
  InternalSetSourceList(SourceList);
end;

procedure TBaseAureliusDataset.SetSourceObject(SourceObject: TObject);
begin
  CheckInactive;
  FInternalList.Clear;
  FInternalList.Add(SourceObject);
  InternalSetSourceList(FInternalList);
  ObjectClass := SourceObject.ClassType;
end;

procedure TBaseAureliusDataset.SplitProp(const AText: string; var AProp, ASubProp: string);
var
  p: integer;
begin
  p := Pos('.', AText);
  if p = 0 then
  begin
    AProp := AText;
    ASubProp := '';
  end else
  begin
    AProp := Copy(AText, 1, p - 1);
    ASubProp := Copy(AText, p + 1, MaxInt);
  end;
end;

procedure TBaseAureliusDataset.UpdateListFromParent(Field: TDatasetField);
var
  List: TObject;
  Parent: TBaseAureliusDataset;
  MasterObject: TObject;
  O: TRttiOptimization;
  ListClass: TClass;
begin
  if Field = nil then Exit;
  Parent := Field.DataSet as TBaseAureliusDataset;
  MasterObject := Parent.Current<TObject>;
  if MasterObject <> nil then
    List := VariantToObject(GetPropValue(Field.FieldName, MasterObject))
  else
    List := nil;

  if List <> nil then
  begin
    InternalSetSourceList(List);
  end
  else
  begin
    InternalSetSourceList(FInternalList);
    if Parent.ObjectClass <> nil then
    begin
      O := FindPropInfoByName(Parent.ObjectClass, Field.FieldName);
      if O <> nil then
      begin
        ListClass := O.MemberClass;
        ObjectClass := TRttiUtils.GetInstance.GetSurroundedClass(ListClass);
      end else
        ObjectClass := TObject;
    end;
  end;
end;

class function TBaseAureliusDataset.VariantToObject(V: Variant): TObject;
begin
  if VarIsNull(V) then Exit(nil);
  {$IFDEF DELPHIXE2_LVL}
  Result := TObject(IntPtr(V));
  {$ELSE}
  Result := TObject(integer(V));
  {$ENDIF}
end;

{ TCustomAureliusDataset }

constructor TCustomAureliusDataset.Create(AOwner: TComponent);
begin
  inherited;
end;

function TCustomAureliusDataset.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  Result := inherited CreateNestedDataSet(DataSetField);
end;

destructor TCustomAureliusDataset.Destroy;
begin
  inherited;
end;

function TCustomAureliusDataset.GetExplorer: TMappingExplorer;
begin
  if FManager <> nil then
    Result := FManager.Explorer
  else
    Result := inherited;
end;

procedure TCustomAureliusDataset.InternalObjectInsert(Obj: TObject);
begin
  if FManager <> nil then
  begin
    FManager.Save(Obj);
    FManager.Flush;
  end;
end;

procedure TCustomAureliusDataset.InternalObjectRemove(Obj: TObject);
begin
  if FManager <> nil then
  begin
    FManager.Remove(Obj);
    FManager.Flush;
  end;
end;

procedure TCustomAureliusDataset.InternalObjectUpdate(Obj: TObject);
begin
  if FManager <> nil then
  begin
    FManager.SaveOrUpdate(Obj);
    FManager.Flush;
  end;
end;

procedure TCustomAureliusDataset.SetDataSetField(const Value: TDataSetField);
begin
  inherited;
  if (Value <> nil) then
    FManager := TCustomAureliusDataset(Value.Dataset).Manager
  else
    FManager := nil;
end;

procedure TCustomAureliusDataset.SetManager(const Value: TObjectManager);
var
  I: Integer;
begin
  if FManager <> Value then
  begin
    CheckInactive;
    FManager := Value;
    for I := 0 to NestedDataSets.Count - 1 do
      TCustomAureliusDataset(NestedDatasets[I]).Manager := Manager;
  end;
end;

{ TObjBlobStream }

constructor TObjBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FField := Field;
  FFieldNo := FField.FieldNo;
  FDataSet := FField.DataSet as TBaseAureliusDataset;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not(FDataSet.State in [dsEdit, dsInsert, dsNewValue]) then
      DatabaseError(SNotEditing, FDataSet);
  end;
  FBuffer := FDataSet.GetActiveRecBuf;
  if FBuffer = TBaseAureliusDataset.NullBuffer then
    Exit;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TObjBlobStream.Destroy;
var
  LocalBytes: TBytes;
begin
  if FModified then
    try
      SetLength(LocalBytes, Size);
      Move(Self.Memory^, LocalBytes[0], Size);
      FDataset.SetFieldData(FField, LocalBytes);

//      FDataset.SetFieldData(FField, @LocalBytes[0]);

//      case FField.DataType of
//        ftMemo: data.db
//          VariantData := AnsiString(StringOf(LocalBytes));
//        ftWideMemo:
//          VariantData := WideStringOf(LocalBytes);
//      else
//        VariantData := TUtils.BytesToVariant(LocalBytes);
//      end;
//      FDataSet.SetFieldData(FField, @VariantData);

      FField.Modified := True;
      FDataSet.DataEvent(deFieldChange, IntPtr(FField));
    except
      if Assigned(Classes.ApplicationHandleException) then
        ApplicationHandleException(Self);
    end;
  inherited Destroy;
end;

procedure TObjBlobStream.ReadBlobData;
var
  LocalBytes: TBytes;
begin
  LocalBytes := FDataSet.GetBlobData(FField);
  Clear;
  Write(LocalBytes[0], Length(LocalBytes));
  Position := 0;
  FModified := false;
end;

procedure TObjBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;

function TObjBlobStream.Write(const Buffer; Count: integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

{ TEntityField }

function TAureliusEntityField.AsEntity<T>: T;
begin
  Result := T(AsObject);
end;

function TAureliusEntityField.GetAsObject: TObject;
begin
  Result := TAureliusDataset.VariantToObject(AsVariant);
end;

{$IFDEF DELPHIXE3_LVL}
function TAureliusEntityField.GetAsVariant: Variant;
var
  TempBuff: TValueBuffer;
begin
  SetLength(TempBuff, SizeOf(Variant));
  if GetData(TempBuff) then
    // TBitConverter.ToVariant is wrong! (buggy)
    //Result := TBitConverter.ToVariant(TempBuff)
    System.Move(TempBuff[0], Result, SizeOf(Variant))
  else
    Result := Null;
end;
{$ENDIF}

{$IFDEF DELPHIXE3_LVL}
procedure TAureliusEntityField.SetVarValue(const Value: Variant);
begin
  SetData(BytesOf(@Value, SizeOf(Variant)));
end;
{$ENDIF}

procedure TAureliusEntityField.SetAsObject(const Value: TObject);
begin
  {$IFDEF NEXTGEN}
  FObject := Value;
  {$ENDIF}
  AsVariant := TAureliusDataset.ObjectToVariant(Value);
end;

end.
