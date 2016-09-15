unit Aurelius.Commands.Listeners;

{$I Aurelius.inc}

interface

uses
  Generics.Collections,
  Aurelius.Drivers.Interfaces;

type
  ICommandExecutionListener = interface
    procedure ExecutingCommand(SQL: string; Params: TEnumerable<TDBParam>);
  end;

  TTextCommandListener = class abstract(TInterfacedObject, ICommandExecutionListener)
  protected
    procedure WriteLine(Line: string); virtual; abstract;
  public
    procedure ExecutingCommand(SQL: string; Params: TEnumerable<TDBParam>); virtual;
  end;

  TConsoleOutputCommandEngine = class(TTextCommandListener)
  protected
    procedure WriteLine(Line: string); override;
  public
    constructor Create; virtual;
  end;

implementation

uses
  Variants, DB, SysUtils, TypInfo;

{ TTextCommandListener }

procedure TTextCommandListener.ExecutingCommand(SQL: string;
  Params: TEnumerable<TDBParam>);
var
  P: TDBParam;
  ValueAsString: string;
begin
  WriteLine(SQL);

  if Params <> nil then
  begin
    WriteLine('');

    for P in Params do
    begin
      if P.ParamValue = Variants.Null then
        ValueAsString := 'NULL'
      else
      if P.ParamType = ftDateTime then
        ValueAsString := '"' + DateTimeToStr(P.ParamValue) + '"'
      else
      if P.ParamType = ftDate then
        ValueAsString := '"' + DateToStr(P.ParamValue) + '"'
      else
        ValueAsString := '"' + VarToStr(P.ParamValue) + '"';

      WriteLine(P.ParamName + ' = ' + ValueAsString + ' (' +
        GetEnumName(TypeInfo(TFieldType), Ord(P.ParamType)) + ')');
    end;
  end;

  WriteLine('');
  WriteLine('================================================');
end;

{ TConsoleOutputCommandEngine }

constructor TConsoleOutputCommandEngine.Create;
begin
  if not IsConsole then
  begin
    raise Exception.Create('Application must have console.');
  end;
end;

procedure TConsoleOutputCommandEngine.WriteLine(Line: string);
begin
  System.Writeln(Line);
end;

end.
