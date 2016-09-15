unit Aurelius.Design.DatasetDesigner;

interface

uses
  Generics.Collections, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSDesign, StdCtrls, DB,
  Aurelius.Bind.Dataset, CheckLst, Buttons;

type
  TfmFieldLoader = class(TForm)
    btOk: TButton;
    btCancel: TButton;
    cbClasses: TComboBox;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    lbPackages: TCheckListBox;
    btAddPackage: TSpeedButton;
    btRemovePackage: TSpeedButton;
    edPath: TEdit;
    procedure btOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btAddPackageClick(Sender: TObject);
    procedure btRemovePackageClick(Sender: TObject);
    procedure lbPackagesClick(Sender: TObject);
    procedure lbPackagesClickCheck(Sender: TObject);
  private
    type
      TPackageInfo = class
      private
        Name: string;
        FileName: string;
        Description: string;
        Handle: HMODULE;
        Owner: TfmFieldLoader;
      public
        constructor Create(AOwner: TfmFieldLoader);
        destructor Destroy; override;
        procedure Load;
        procedure Unload;
      end;
  private
    FDataset: TDataset;
    FPackages: TObjectList<TPackageInfo>;
    function PackageKey: string;
    procedure FillClassesCombo;
    function PackageExists(AFileName: string): boolean;
    function AddPackage(AFileName: string): TPackageInfo;
    procedure RemovePackage(APackage: TPackageInfo);
    function GetPackageInList(AIndex: integer): TPackageInfo;
    procedure FillPackageList(ToSelect: TPackageInfo);
    procedure LoadPackageList;
    procedure SavePackageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(Dataset: TDataset): boolean;
  end;

type
  TObjectDatasetDesigner = class(TDSDesigner)
  private
    FSaveFieldDefs: TAureliusDataset;
  public
    constructor Create(DataSet: TDataSet); override;
    destructor Destroy; override;
    procedure BeginUpdateFieldDefs; override;
    procedure EndUpdateFieldDefs; override;
  end;

implementation
uses
  Rtti, Registry,
  Aurelius.Mapping.Attributes,
  Aurelius.Mapping.Explorer;

{$R *.dfm}

type
  TInternalAureliusDataset = class(TBaseAureliusDataset)
  end;

{ TfmFieldLoader }

function TfmFieldLoader.AddPackage(AFileName: string): TPackageInfo;
var
  P: TPackageInfo;
begin
  P := TPackageInfo.Create(Self);
  try
    try
      P.Description := GetPackageDescription(PChar(AFileName));
    except
      P.Description := '';
    end;
    P.Name := ExtractFileName(AFileName);
    P.FileName := AFileName;
    FPackages.Add(P);
  except
    P.Free;
  end;
  Result := P;
end;

procedure TfmFieldLoader.btAddPackageClick(Sender: TObject);
var
  New: TPackageInfo;
begin
  if OpenDialog1.Execute then
  begin
    if not PackageExists(OpenDialog1.FileName) then
    begin
      New := AddPackage(OpenDialog1.FileName);
      try
        New.Load;
      except
        RemovePackage(New);
        raise;
      end;
      FillPackageList(New);
    end;
  end;
end;

procedure TfmFieldLoader.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmFieldLoader.btOkClick(Sender: TObject);
var
  SelectedClass: TClass;
begin
  if cbClasses.ItemIndex >= 0 then
  begin
    SelectedClass := TClass(Pointer(cbClasses.Items.Objects[cbClasses.ItemIndex]));
    TInternalAureliusDataset(FDataset).InitFieldDefsFromClass(SelectedClass);
  end;
  SavePackageList;
  ModalResult := mrOk;
end;

procedure TfmFieldLoader.btRemovePackageClick(Sender: TObject);
begin
  if lbPackages.ItemIndex >= 0 then
    RemovePackage(GetPackageInList(lbPackages.ItemIndex));
end;

constructor TfmFieldLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPackages := TObjectList<TPackageInfo>.Create;
end;

destructor TfmFieldLoader.Destroy;
begin
  // Clear all cache in TMappingExplorer, because we are unloading the packages,
  // there might be some garbage classes cached in rtti explorer
  // TODO: A better way of handling this is just use a local instance of TMappingExplorer,
  // and pass it to the TAureliusdataset so the defaultinstance is not touched by the dataset when the fields are retrieved
  TMappingExplorer.ReplaceDefaultInstance(nil);
  FPackages.Free;
  inherited;
end;

class function TfmFieldLoader.Execute(Dataset: TDataset): boolean;
var
  Form: TfmFieldLoader;
begin
  Form := TfmFieldLoader.Create(Application);
  try
    Form.FDataset := Dataset;
    Result := (Form.ShowModal = mrOk);
  finally
    Form.Free;
  end;
end;

procedure TfmFieldLoader.FillClassesCombo;
var
  Context: TRttiContext;
  AllTypes: TArray<TRttiType>;
  T: TRttiType;
  A: TCustomAttribute;
  EntityClass: TClass;
begin
  if csDestroying in ComponentState then Exit;
  
  cbClasses.Clear;
  cbClasses.Sorted := false;
  Context := TRttiContext.Create;
  try
    AllTypes := Context.GetTypes;
    for T in AllTypes do
      if T.IsInstance then
        for A in T.GetAttributes do
          if A is Entity then
          begin
            EntityClass := T.AsInstance.MetaclassType;
            cbClasses.Items.AddObject(EntityClass.ClassName, TObject(Pointer(EntityClass)))
          end;
    cbClasses.Sorted := true;
  finally
    Context.Free;
  end;
end;

procedure TfmFieldLoader.FillPackageList(ToSelect: TPackageInfo);
var
  P: TPackageInfo;
  List: TStrings;
  Str: string;
begin
  List := lbPackages.Items;
  List.BeginUpdate;
  try
    List.Clear;
    lbPackages.Sorted := false;
    for P in FPackages do
    begin
      if P.Description <> '' then
        Str := Format('%s (%s)', [P.Description, P.Name])
      else
        Str := P.FileName;
      List.AddObject(Str, P);
      lbPackages.Checked[List.Count - 1] := P.Handle <> 0;
    end;
    lbPackages.Sorted := true;
    if ToSelect <> nil then
      lbPackages.ItemIndex := lbPackages.Items.IndexOfObject(ToSelect);
    lbPackagesClick(nil);
  finally
    List.EndUpdate;
  end;
end;

procedure TfmFieldLoader.FormCreate(Sender: TObject);
begin
  LoadPackageList;
  FillClassesCombo;
end;

function TfmFieldLoader.GetPackageInList(AIndex: integer): TPackageInfo;
begin
  if (AIndex < 0) or (AIndex >= lbPackages.Count) then Exit(nil);
  Result := TPackageInfo(lbPackages.Items.Objects[AIndex]);
end;

procedure TfmFieldLoader.lbPackagesClick(Sender: TObject);
var
  P: TPackageInfo;
begin
  P := GetPackageInList(lbPackages.ItemIndex);
  if P <> nil then
    edPath.Text := P.FileName
  else
    edPath.Text := '';
end;

procedure TfmFieldLoader.lbPackagesClickCheck(Sender: TObject);
begin
  if (lbPackages.ItemIndex < 0) or (lbPackages.ItemIndex >= lbPackages.Count) then Exit;
  if lbPackages.Checked[lbPackages.ItemIndex] then
    GetPackageInList(lbPackages.ItemIndex).Load
  else
    GetPackageInList(lbPackages.ItemIndex).Unload;
end;

procedure TfmFieldLoader.LoadPackageList;
var
  R: TRegIniFile;
  Sections: TStringList;
  P: TPackageInfo;
  I: integer;
  FileName: string;
  Loaded: boolean;
begin
  R := TRegIniFile.Create(PackageKey);
  Sections := TStringList.Create;
  try
    R.ReadSections(Sections);
    FPackages.Clear;
    for I := 0 to Sections.Count - 1 do
    begin
      FileName := R.ReadString(Sections[I], 'FileName', '');
      if FileName <> '' then
      begin
        Loaded := R.ReadBool(Sections[I], 'Loaded', false);
        P := AddPackage(FileName);
        if Loaded then
          try
            P.Load;
          except
          end;
      end;
    end;
    FillPackageList(nil);
  finally
    Sections.Free;
    R.Free;
  end;
end;

function TfmFieldLoader.PackageExists(AFileName: string): boolean;
var
  P: TPackageInfo;
begin
  for P in FPackages do
    if SameText(AFileName, P.FileName) then
      Exit(true);
  Exit(false);
end;

function TfmFieldLoader.PackageKey: string;
var
  DelphiIDE: string;
begin
  {$IF DEFINED(VER210)}
  DelphiIDE := 'IDE\2010';
  {$ELSEIF Defined(VER220)}
  DelphiIDE := 'IDE\XE';
  {$ELSEIF Defined(VER230)}
  DelphiIDE := 'IDE\XE2';
  {$ELSEIF Defined(VER240)}
  DelphiIDE := 'IDE\XE3';
  {$ELSEIF Defined(VER250)}
  DelphiIDE := 'IDE\XE4';
  {$ELSEIF Defined(VER260)}
  DelphiIDE := 'IDE\XE5';
  {$ELSEIF Defined(VER270)}
   DelphiIDE := 'IDE\XE6';
    {$ELSEIF Defined(VER280)}
   DelphiIDE := 'IDE\XE7';
  {$ELSE}
  Compiler error. Support this new Delphi.
  {$IFEND}

  Result := Format('Software\TMS Software\Aurelius\%s\DatasetPackages', [DelphiIDE]);
end;

procedure TfmFieldLoader.RemovePackage(APackage: TPackageInfo);
begin
  FPackages.Remove(APackage);
  FillPackageList(nil);
end;

procedure TfmFieldLoader.SavePackageList;
var
  R: TRegIniFile;
  P: TPackageInfo;
  Sections: TStringList;
  I: Integer;
begin
  R := TRegIniFile.Create(PackageKey);
  Sections := TStringList.Create;
  try
    R.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
      R.EraseSection(Sections[I]);
    for P in FPackages do
    begin
      R.WriteString(P.Name, 'FileName', P.FileName);
      R.WriteBool(P.Name, 'Loaded', P.Handle <> 0);
    end;
  finally
    Sections.Free;
    R.Free;
  end;
end;

{ TObjectDatasetDesigner }

procedure TObjectDatasetDesigner.BeginUpdateFieldDefs;
begin
  // Do not update field defs. Just save what's there and retrieve it back after update
  // This code, together with EndUpdateFieldDefs, just avoids that FieldDefs are updated by the designer
  if Dataset <> nil then
    FSaveFieldDefs.FieldDefs.Assign(Dataset.FieldDefs);
  inherited;
end;

constructor TObjectDatasetDesigner.Create(DataSet: TDataSet);
begin
  inherited;
  FSaveFieldDefs := TAureliusDataset.Create(nil);
end;

destructor TObjectDatasetDesigner.Destroy;
begin
  FSaveFieldDefs.Free;
  inherited;
end;

procedure TObjectDatasetDesigner.EndUpdateFieldDefs;
begin
  inherited;
  if Dataset <> nil then
    Dataset.FieldDefs.Assign(FSaveFieldDefs.FieldDefs);
end;

{ TfmFieldLoader.TPackageInfo }

constructor TfmFieldLoader.TPackageInfo.Create(AOwner: TfmFieldLoader);
begin
  Owner := AOwner;
end;

destructor TfmFieldLoader.TPackageInfo.Destroy;
begin
  Unload;
  inherited;
end;

procedure TfmFieldLoader.TPackageInfo.Load;
begin
  if Handle = 0 then
  begin
    Handle := LoadPackage(FileName);
    Owner.FillPackageList(Self);
    Owner.FillClassesCombo;
  end;
end;

procedure TfmFieldLoader.TPackageInfo.Unload;
begin
  if Handle <> 0 then
  begin
    UnloadPackage(Handle);
    Handle := 0;
    Owner.FillPackageList(Self);
    Owner.FillClassesCombo;
  end;
end;

end.
