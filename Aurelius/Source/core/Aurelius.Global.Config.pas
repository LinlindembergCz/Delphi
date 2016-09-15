unit Aurelius.Global.Config;

{$I Aurelius.inc}

interface

uses
  Aurelius.Mapping.Metadata;

type
  TAutomappingMode = (Off, ByClass, Full);

  TGlobalConfigs = class
  private
    class var FInstance: TGlobalConfigs;
  private
    FMapStringToNationalChar: boolean;
    FSimuleStatements: Boolean;
    FMaxEagerFetchDepth: Integer;
    FTightStringEnumLength: Boolean;
    FAutoMappingMode: TAutomappingMode;
    FAutoMappingDefaultCascade: TCascadeTypes;
    FDefaultStringColWidth: Integer;
    FAutoMappingDefaultCascadeManyValued: TCascadeTypes;
    procedure Load;
  public
    class function GetInstance: TGlobalConfigs;

    // If true, all statements are not executed on the DBMS, but appear in the
    // listeners.
    property SimuleStatements: Boolean read FSimuleStatements write FSimuleStatements;

    // Indicates the maximum depth to load objects in eager loading
    // associations. Beyond this depth, the objects still loading in lazy mode.
    property MaxEagerFetchDepth: Integer read FMaxEagerFetchDepth write FMaxEagerFetchDepth;

    // If true, in enums mapped to string columns with no length specified
    // in the [Column()] mapping will generate the column length equal to
    // the largest possible value of the enum.
    // Otherwise, the length is DefaultStringColWidth by default
    // (When not specified in [Column()]).
    property TightStringEnumLength: Boolean read FTightStringEnumLength write FTightStringEnumLength;

    // Defines the automapping mode:
    // amOff: No automatic mapping. Only elements with attributes are mapped.
    // amByClass: Automapping is done over classes with [Automapping] attribute.
    // amFull: Full automapping over every registered class.
    property AutoMappingMode: TAutomappingMode read FAutoMappingMode write FAutoMappingMode;

    // If AutoMapping is enabled, defines the default cascade type for all
    // automapped associations
    property AutoMappingDefaultCascade: TCascadeTypes read FAutoMappingDefaultCascade write FAutoMappingDefaultCascade;

    // If AutoMapping is enabled, defines the default cascade type for all
    // automapped many-valued associations
    property AutoMappingDefaultCascadeManyValued: TCascadeTypes read FAutoMappingDefaultCascadeManyValued write FAutoMappingDefaultCascadeManyValued;

    // Defines the width for string (usually varchar) columns when the width
    // was not particularly specified.
    property DefaultStringColWidth: Integer read FDefaultStringColWidth write FDefaultStringColWidth;

    // If enabled, resolve string field types as NVARCHAR instead of VARCHAR
    property MapStringToNationalChar: boolean read FMapStringToNationalChar write FMapStringToNationalChar;
  end;

implementation

{ TGlobalConfigs }

class function TGlobalConfigs.GetInstance: TGlobalConfigs;
begin
  if FInstance = nil then
  begin
    FInstance := TGlobalConfigs.Create;
    FInstance.Load;
  end;
  Result := FInstance;
end;

procedure TGlobalConfigs.Load;
begin
  // TODO: Implement configuration load

  FSimuleStatements := False;
  FMaxEagerFetchDepth := 3;
  FTightStringEnumLength := True;
  FAutoMappingMode := TAutomappingMode.ByClass;
  FAutoMappingDefaultCascade := CascadeTypeAll - [TCascadeType.Remove];
  FAutoMappingDefaultCascadeManyValued := CascadeTypeAll;
  FDefaultStringColWidth := 255;
  FMapStringToNationalChar := false;
end;

initialization

finalization

TGlobalConfigs.FInstance.Free;
TGlobalConfigs.FInstance := nil;

end.
