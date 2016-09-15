unit Aurelius.Json.Settings;

{$I Aurelius.Inc}

interface

type
  TJsonSettings = class
  public
    type
      TEnumMode = (Text, Ordinal);
  strict private
    const
      DefaultRefIdProperty = '$id';
      DefaultRefProperty = '$ref';
      DefaultTypeProperty = '$type';
      DefaultTypeSeparator = '.';
      DefaultEnumMode = TEnumMode.Text;
      DefaultProxyKeyProperty = 'key';
      DefaultProxyListIndicator = 'list';
      DefaultProxySingleIndicator = 'single';
      DefaultProxyProperty = '$proxy';
      DefaultProxyMemberProperty = 'member';
      DefaultProxyClassProperty = 'class';
      DefaultBlobKeyProperty = 'key';
      DefaultBlobProperty = '$lazyblob';
      DefaultBlobMemberProperty = 'member';
      DefaultBlobClassProperty = 'class';
  private
    FRefProperty: string;
    FTypeProperty: string;
    FEnumMode: TEnumMode;
    FTypeSeparator: string;
    FRefIdProperty: string;
    FProxyKeyProperty: string;
    FProxyListIndicator: string;
    FProxySingleIndicator: string;
    FProxyProperty: string;
    FProxyMemberProperty: string;
    FProxyClassProperty: string;
    FBlobKeyProperty: string;
    FBlobProperty: string;
    FBlobMemberProperty: string;
    FBlobClassProperty: string;
  public
    constructor Create;
    property RefProperty: string read FRefProperty write FRefProperty;
    property RefIdProperty: string read FRefIdProperty write FRefIdProperty;
    property TypeProperty: string read FTypeProperty write FTypeProperty;
    property TypeSeparator: string read FTypeSeparator;
    property EnumMode: TEnumMode read FEnumMode write FEnumMode;
    property ProxyProperty: string read FProxyProperty;
    property ProxyKeyProperty: string read FProxyKeyProperty;
    property ProxyClassProperty: string read FProxyClassProperty;
    property ProxyMemberProperty: string read FProxyMemberProperty;
    property ProxyListIndicator: string read FProxyListIndicator;
    property ProxySingleIndicator: string read FProxySingleIndicator;
    property BlobProperty: string read FBlobProperty;
    property BlobKeyProperty: string read FBlobKeyProperty;
    property BlobClassProperty: string read FBlobClassProperty;
    property BlobMemberProperty: string read FBlobMemberProperty;
  end;

implementation

{ TJsonSettings }

constructor TJsonSettings.Create;
begin
  FEnumMode := DefaultEnumMode;
  FRefProperty := DefaultRefProperty;
  FRefIdProperty := DefaultRefIdProperty;
  FTypeProperty := DefaultTypeProperty;
  FTypeSeparator := DefaultTypeSeparator;

  FProxyKeyProperty := DefaultProxyKeyProperty;
  FProxyListIndicator := DefaultProxyListIndicator;
  FProxySingleIndicator := DefaultProxySingleIndicator;
  FProxyProperty := DefaultProxyProperty;
  FProxyMemberProperty := DefaultProxyMemberProperty;
  FProxyClassProperty := DefaultProxyClassProperty;

  FBlobKeyProperty := DefaultBlobKeyProperty;
  FBlobProperty := DefaultBlobProperty;
  FBlobMemberProperty := DefaultBlobMemberProperty;
  FBlobClassProperty := DefaultBlobClassProperty;
end;

end.
