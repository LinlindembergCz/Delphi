unit Aurelius.Schema.Commands;

{$I Aurelius.Inc}

interface

uses
  Aurelius.Sql.Metadata,
  Aurelius.Sql.Interfaces;

type
  TDDLCommand = class abstract
  public
    function BuildSQL(Generator: ISQLGenerator): string; virtual; abstract;
  end;

  TCreateTableCommand = class(TDDLCommand)
  strict private
    FTable: TTableMetadata;
    FCreateForeignKeys: boolean;
  public
    constructor Create(ATable: TTableMetadata; ACreateForeignKeys: boolean);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Table: TTableMetadata read FTable write FTable;
    property CreateForeignKeys: boolean read FCreateForeignKeys write FCreateForeignKeys;
  end;

  TCreateSequenceCommand = class(TDDLCommand)
  strict private
    FSequence: TSequenceMetadata;
  public
    constructor Create(ASequence: TSequenceMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Sequence: TSequenceMetadata read FSequence write FSequence;
  end;

  TCreateColumnCommand = class(TDDLCommand)
  strict private
    FColumn: TColumnMetadata;
  public
    constructor Create(AColumn: TColumnMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Column: TColumnMetadata read FColumn write FColumn;
  end;

  TCreateForeignKeyCommand = class(TDDLCommand)
  strict private
    FForeignKey: TForeignKeyMetadata;
  public
    constructor Create(AForeignKey: TForeignKeyMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property ForeignKey: TForeignKeyMetadata read FForeignKey write FForeignKey;
  end;

//  TCreateUniqueKeyCommand = class(TDDLCommand)
//  strict private
//    FUniqueKey: TUniqueKeyMetadata;
//  public
//    constructor Create(AUniqueKey: TUniqueKeyMetadata);
//    property UniqueKey: TUniqueKeyMetadata read FUniqueKey write FUniqueKey;
//  end;

  TDropTableCommand = class(TDDLCommand)
  strict private
    FTable: TTableMetadata;
  public
    constructor Create(ATable: TTableMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Table: TTableMetadata read FTable write FTable;
  end;

  TDropSequenceCommand = class(TDDLCommand)
  strict private
    FSequence: TSequenceMetadata;
  public
    constructor Create(ASequence: TSequenceMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Sequence: TSequenceMetadata read FSequence write FSequence;
  end;

  TDropForeignKeyCommand = class(TDDLCommand)
  strict private
    FForeignKey: TForeignKeyMetadata;
  public
    constructor Create(AForeignKey: TForeignKeyMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property ForeignKey: TForeignKeyMetadata read FForeignKey write FForeignKey;
  end;

  TDropColumnCommand = class(TDDLCommand)
  strict private
    FColumn: TColumnMetadata;
  public
    constructor Create(AColumn: TColumnMetadata);
    property Column: TColumnMetadata read FColumn write FColumn;
  end;

  TDropUniqueKeyCommand = class(TDDLCommand)
  strict private
    FUniqueKey: TUniqueKeyMetadata;
  public
    constructor Create(AUniqueKey: TUniqueKeyMetadata);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Uniquekey: TUniqueKeyMetadata read FUniqueKey write FUniqueKey;
  end;

  TEnableForeignKeysCommand = class(TDDLCommand)
  strict private
    FEnable: boolean;
  public
    constructor Create(AEnable: boolean);
    function BuildSQL(Generator: ISQLGenerator): string; override;
    property Enable: boolean read FEnable write FEnable;
  end;

implementation

//{ TCreateUniqueKeyCommand }
//
//constructor TCreateUniqueKeyCommand.Create(AUniqueKey: TUniqueKeyMetadata);
//begin
//  FUniqueKey := AUniqueKey;
//end;

{ TCreateTableCommand }

function TCreateTableCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateCreateTable(Self.Table, Self.CreateForeignKeys);
end;

constructor TCreateTableCommand.Create(ATable: TTableMetadata; ACreateForeignKeys: boolean);
begin
  FTable := ATable;
  FCreateForeignKeys := ACreateForeignKeys;
end;

{ TDropSequenceCommand }

function TDropSequenceCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateDropSequence(Self.Sequence);
end;

constructor TDropSequenceCommand.Create(ASequence: TSequenceMetadata);
begin
  FSequence := ASequence;
end;

{ TDropForeignKeyCommand }

function TDropForeignKeyCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateDropForeignKey(Self.ForeignKey);
end;

constructor TDropForeignKeyCommand.Create(AForeignKey: TForeignKeyMetadata);
begin
  FForeignKey := AForeignKey;
end;

{ TCreateColumnCommand }

function TCreateColumnCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateCreateColumn(Self.Column);
end;

constructor TCreateColumnCommand.Create(AColumn: TColumnMetadata);
begin
  FColumn := AColumn;
end;

{ TDropColumnCommand }

constructor TDropColumnCommand.Create(AColumn: TColumnMetadata);
begin
  FColumn := AColumn;
end;

{ TCreateForeignKeyCommand }

function TCreateForeignKeyCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateCreateForeignKey(Self.ForeignKey);
end;

constructor TCreateForeignKeyCommand.Create(AForeignKey: TForeignKeyMetadata);
begin
  FForeignKey := AForeignKey;
end;

{ TCreateSequenceCommand }

function TCreateSequenceCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateCreateSequence(Self.Sequence);
end;

constructor TCreateSequenceCommand.Create(ASequence: TSequenceMetadata);
begin
  FSequence := ASequence;
end;

{ TDropTableCommand }

function TDropTableCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateDropTable(Self.Table);
end;

constructor TDropTableCommand.Create(ATable: TTableMetadata);
begin
  FTable := ATable;
end;

{ TEnableForeignKeysCommand }

function TEnableForeignKeysCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateEnableForeignKeys(Self.Enable);
end;

constructor TEnableForeignKeysCommand.Create(AEnable: boolean);
begin
  FEnable := AEnable;
end;

{ TDropUniqueKeyCommand }

function TDropUniqueKeyCommand.BuildSQL(Generator: ISQLGenerator): string;
begin
  Result := Generator.GenerateDropUniqueKey(Self.Uniquekey);
end;

constructor TDropUniqueKeyCommand.Create(AUniqueKey: TUniqueKeyMetadata);
begin
  FUniqueKey := AUniqueKey;
end;

end.
