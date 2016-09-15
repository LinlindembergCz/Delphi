unit Aurelius.Types.Blob;

{$I Aurelius.inc}

interface
uses
  Classes,
  SysUtils;

type
  IBlobInfo = interface
    function ClassName: string;
    function MemberName: string;
    function Key: Variant;
  end;

  IBlobController = interface
    function ReadBlob: TArray<byte>;
    function BlobInfo: IBlobInfo;
  end;

  TBlob = record
  private
    FIntData: TArray<byte>;
    FController: IBlobController;
    FLoaded: Boolean;
    procedure CheckLoaded;
    function GetSize: integer;
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetData: PByte;
    function GetIsNull: boolean;
    procedure SetIsNull(const Value: boolean);
  public
    constructor Create(Value: TBytes); overload;
    class operator Implicit(Value: TBlob): TBytes;
    class operator Implicit(Value: TBytes): TBlob;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property Loaded: boolean read FLoaded;
    property Data: PByte read GetData;
    property Size: integer read GetSize;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsString: string read GetAsString write SetAsString;
  end;

implementation

{ TBlob }

procedure TBlob.CheckLoaded;
begin
  if FLoaded then Exit;
  if FController <> nil then
    FIntData := FController.ReadBlob;
  FLoaded := True;
end;

procedure TBlob.Clear;
begin
  IsNull := true;
end;

constructor TBlob.Create(Value: TBytes);
begin
  AsBytes := Value;
end;

function TBlob.GetAsBytes: TBytes;
begin
  SetLength(Result, Size);
  System.Move(Data^, Result[0], Size);
end;

function TBlob.GetAsString: string;
begin
  Result := StringOf(AsBytes);
end;

function TBlob.GetData: PByte;
begin
  CheckLoaded;
  Result := @FIntData[0];
end;

function TBlob.GetIsNull: boolean;
begin
  Result := (Size = 0);
end;

function TBlob.GetSize: Longint;
begin
  CheckLoaded;
  Result := Length(FIntData);
end;

class operator TBlob.Implicit(Value: TBlob): TBytes;
begin
  Result := Value.AsBytes;
end;

class operator TBlob.Implicit(Value: TBytes): TBlob;
begin
  Result.AsBytes := Value;
end;

procedure TBlob.LoadFromStream(Stream: TStream);
var
  Count: Longint;
begin
  //Stream.Position := 0;
  Count := Stream.Size - Stream.Position;
  SetLength(FIntData, Count);
  Stream.Read(FIntData[0], Count);
  FLoaded := True;
end;

procedure TBlob.SaveToStream(Stream: TStream);
begin
  Stream.Write(Data^, Size);
end;

procedure TBlob.SetAsBytes(const Value: TBytes);
begin
  SetLength(FIntData, Length(Value));
  System.Move(Value[0], FIntData[0], Size);
  FLoaded := True;
end;

procedure TBlob.SetAsString(const Value: string);
begin
  AsBytes := BytesOf(Value);
end;

procedure TBlob.SetIsNull(const Value: boolean);
begin
  SetLength(FIntData, 0);
  FLoaded := True;
end;

end.
