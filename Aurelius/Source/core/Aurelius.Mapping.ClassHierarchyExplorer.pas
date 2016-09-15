unit Aurelius.Mapping.ClassHierarchyExplorer;

{$I Aurelius.inc}

interface

uses
  Generics.Collections;

type
  TClassHierarchyExplorer = class
  private
    FDirectSubClasses: TDictionary<TClass, TList<TClass>>;
    FAllSubClasses: TDictionary<TClass, TList<TClass>>;
    function FindSubClasses(Clazz: TClass): TList<TClass>;
    function GetClasses: TEnumerable<TClass>;
  public
    constructor Create(AClasses: TArray<TClass>); overload;
    constructor Create(AClasses: TEnumerable<TClass>); overload;
    destructor Destroy; override;

    // TODO: Improve to get only concrete classes
    function GetDirectSubClasses(Clazz: TClass): TEnumerable<TClass>;
    function GetAllSubClasses(Clazz: TClass): TEnumerable<TClass>;
    function FindClassByName(ClassName: string): TClass;
    function Contains(Clazz: TClass): boolean;
    property Classes: TEnumerable<TClass> read GetClasses;
  end;

implementation

uses
  SysUtils, Rtti,
  Aurelius.Mapping.Exceptions;

{ TClassHierarchyExplorer }

constructor TClassHierarchyExplorer.Create(AClasses: TArray<TClass>);
var
  ClassesList: TList<TClass>;
  C: TClass;
begin
  // Could be more optimized but we will keep it because it's not a critical code
  ClassesList := TList<TClass>.Create;
  try
    for C in AClasses do
      ClassesList.Add(C);
    Create(ClassesList);
  finally
    ClassesList.Free;
  end;
end;

function TClassHierarchyExplorer.Contains(Clazz: TClass): boolean;
begin
  Result := FDirectSubClasses.ContainsKey(Clazz);
end;

constructor TClassHierarchyExplorer.Create(AClasses: TEnumerable<TClass>);
var
  C: TClass;
begin
  FDirectSubClasses := TObjectDictionary<TClass, TList<TClass>>.Create([doOwnsValues]);
  FAllSubClasses := TObjectDictionary<TClass, TList<TClass>>.Create([doOwnsValues]);

  // Load direct classes lists
  if AClasses <> nil then
    for C in AClasses do
      if not FDirectSubClasses.ContainsKey(C) then
        FDirectSubClasses.Add(C, TList<TClass>.Create);

  for C in Self.Classes do
    if FDirectSubClasses.ContainsKey(C.ClassParent) then
      FDirectSubClasses[C.ClassParent].Add(C);

  // Load all classes list
  for C in Self.Classes do
    FAllSubClasses.Add(C, FindSubClasses(C));
end;

destructor TClassHierarchyExplorer.Destroy;
begin
  FDirectSubClasses.Free;
  FAllSubClasses.Free;
  inherited;
end;

function TClassHierarchyExplorer.FindClassByName(ClassName: string): TClass;
var
  C: TClass;
begin
  for C in Classes do
    if SameText(C.ClassName, ClassName) then
      Exit(C);
  Result := nil;
end;

function TClassHierarchyExplorer.FindSubClasses(Clazz: TClass): TList<TClass>;
var
  C: TClass;
  SubClassesList: TList<TClass>;
begin
  Result := TList<TClass>.Create;

  Result.AddRange(GetDirectSubClasses(Clazz));

  for C in GetDirectSubClasses(Clazz) do
  begin
    SubClassesList := FindSubClasses(C);
    try
      Result.AddRange(SubClassesList);
    finally
      SubClassesList.Free;
    end;
  end;
end;

function TClassHierarchyExplorer.GetAllSubClasses(
  Clazz: TClass): TEnumerable<TClass>;
begin
  if not FAllSubClasses.ContainsKey(Clazz) then
    raise EClassNotRegistered.Create(Clazz);

  Result := FAllSubClasses[Clazz];
end;

function TClassHierarchyExplorer.GetClasses: TEnumerable<TClass>;
begin
  Result := FDirectSubClasses.Keys;
end;

function TClassHierarchyExplorer.GetDirectSubClasses(
  Clazz: TClass): TEnumerable<TClass>;
begin
  if not FDirectSubClasses.ContainsKey(Clazz) then
    raise EClassNotRegistered.Create(Clazz);

  Result := FDirectSubClasses[Clazz];
end;

end.
