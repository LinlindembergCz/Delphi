unit Aurelius.Drivers.Base;

{$I Aurelius.inc}

interface
uses
  SysUtils, Classes, DB,
  Aurelius.Drivers.Interfaces;

type
  TDriverConnectionAdapter<T: TComponent> = class(TInterfacedObject)
  private
    type
      TNotificationComponent = class(TComponent)
      private
        FAdapter: TDriverConnectionAdapter<T>;
      protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      end;
  private
    FNotification: TNotificationComponent;
    FSqlDialect: string;
    FConnection: T;
    FOwnsConnection: boolean;
  protected
    function Connection: T;
    function RetrieveSqlDialect: string; virtual; abstract;
  public
    constructor Create(AConnection: T; AOwnsConnection: boolean); overload; virtual;
    constructor Create(AConnection: T; ASQLDialect: string; AOwnsConnection: boolean); overload; virtual;
    destructor Destroy; override;
    function SqlDialect: string;
  end;

  TDriverResultSetAdapter<T: TDataset> = class(TInterfacedObject, IDBResultSet)
  private
    FDataset: T;
    FFetching: Boolean;
  protected
    property Dataset: T read FDataset;
  public
    constructor Create(ADataset: T);
    destructor Destroy; override;
    function Next: Boolean;
    function GetFieldValue(FieldName: string): Variant; overload; virtual;
    function GetFieldValue(FieldIndex: Integer): Variant; overload; virtual;
  end;

  TDBConnectionFactory = class(TInterfacedObject, IDBConnectionFactory)
  strict private
    FCreateProc: TFunc<IDBConnection>;
  public
    constructor Create(const ACreateProc: TFunc<IDBConnection>);
    function CreateConnection: IDBConnection;
  end;

implementation

uses
  Variants;

{ TDriverConnectionAdapter<T>.TNotificationComponent }

procedure TDriverConnectionAdapter<T>.TNotificationComponent.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = TComponent(FAdapter.FConnection)) and (Operation = opRemove) then
    FAdapter.FConnection := Default(T);
end;

{ TDriverConnectionAdapter<T> }

function TDriverConnectionAdapter<T>.Connection: T;
begin
  Result := FConnection;
end;

constructor TDriverConnectionAdapter<T>.Create(AConnection: T;
  AOwnsConnection: boolean);
begin
  Create(AConnection, '', AOwnsConnection);
end;

constructor TDriverConnectionAdapter<T>.Create(AConnection: T;
  ASQLDialect: string; AOwnsConnection: boolean);
begin
  FConnection := AConnection;
  FOwnsConnection := AOwnsConnection;
  FSQLDialect := ASQLDialect;
  FNotification := TNotificationComponent.Create(nil);
  FNotification.FAdapter := Self;
  FConnection.FreeNotification(FNotification);
end;

destructor TDriverConnectionAdapter<T>.Destroy;
begin
  FNotification.Free;
  if FOwnsConnection and (FConnection <> nil) then
    FConnection.Free;
  inherited;
end;

function TDriverConnectionAdapter<T>.SqlDialect: string;
begin
  Result := FSqlDialect;
  if Result = '' then
    Result := RetrieveSqlDialect;
end;

{ TDriverResultSetAdapter<T> }

constructor TDriverResultSetAdapter<T>.Create(ADataset: T);
begin
  FDataset := ADataset;
end;

destructor TDriverResultSetAdapter<T>.Destroy;
begin
  FDataset.Free;
  inherited;
end;

function TDriverResultSetAdapter<T>.GetFieldValue(FieldName: string): Variant;
var
  Field: TField;
begin
  Field := FDataset.FieldByName(FieldName);
  Result := GetFieldValue(Field.Index);
end;

function TDriverResultSetAdapter<T>.GetFieldValue(FieldIndex: Integer): Variant;
begin
  if FDataset.Fields[FieldIndex].IsNull then
    Result := Variants.Null
  else
    Result := FDataset.Fields[FieldIndex].Value;
end;

function TDriverResultSetAdapter<T>.Next: Boolean;
begin
  if not FFetching then
    FFetching := True
  else
    FDataset.Next;

  Result := not FDataset.Eof;
end;

{ TDBConnectionFactory }

constructor TDBConnectionFactory.Create(const ACreateProc: TFunc<IDBConnection>);
begin
  FCreateProc := ACreateProc;
end;

function TDBConnectionFactory.CreateConnection: IDBConnection;
begin
  Result := FCreateProc;
end;

end.
