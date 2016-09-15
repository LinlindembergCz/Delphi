unit Aurelius.Global.Utils;

{$I Aurelius.inc}

interface

uses
  SysUtils, Generics.Collections, Rtti;

type
  TUtils = class
  public
    class function ConcatStrings(Strings: TList<string>; Separator: string = ', '; Pattern: string = '%s'): string;
    class function ConcatString(Strings: array of string; Separator: string = ', '; Pattern: string = '%s'): string;

    class function IntToABC(Value: Integer): string;
    class function IntToAlias(Value: Integer): string;

    class function ABCToInt(Value: string): Integer;
    class function AliasToInt(Value: string): Integer;

    // Works also with array of variants
    class function VariantToString(Value: Variant): string;

    class function VariantToValue(Value: Variant): TValue;
    class function ValueToVariant(Value: TValue): Variant;
    class function VariantToBytes(V: Variant): TBytes; static;
    class function BytesToVariant(B: TBytes): Variant; static;

    // Guid functions
    class function VariantToGuid(V: Variant): TGuid; static;
    class function GuidToVariant(G: TGuid): Variant; static;
    class function IsNullGuid(G: TGuid): boolean; static;
    class function NullGuid: TGuid; static;
    class function NewGuid: TGuid; static;

    class function Encode64(Data: TBytes): string; static;
    class function Decode64(S: string): TBytes; static;

    // Not exactly "ISO" but kind of (doesn't use "T" separator or timezone)
    // There functions are subject to change (be removed, refactores, even have their behavior changed
    class function DateTimeToISO(DateValue: TDateTime): string; static;
    class function ISOToDateTime(const Value: string): TDateTime; static;

  end;

implementation

uses
  Variants, DateUtils, DB, Math, SqlTimSt, StrUtils, FmtBCD;

const
  EMPTY_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

{ TUtils }

class function TUtils.ABCToInt(Value: string): Integer;
var
  L: Integer;
  C: Char;
begin
  if Value = '' then
    Exit(-1);

  L := Length(Value);
  C := Value[L];
  if (C < 'A') or (C > 'Z') then
    Exit(-1);

  Result := Ord(C) - Ord('A');
  if L > 1 then
    Inc(Result, 26 * (ABCToInt(Copy(Value, 1, L - 1)) + 1));
end;

class function TUtils.AliasToInt(Value: string): Integer;
begin
  if Value = '' then
    Exit(-1);

  if EndsStr('_', Value) then
    Delete(Value, Length(Value), 1);

  Result := ABCToInt(Value);
end;

class function TUtils.BytesToVariant(B: TBytes): Variant;
var
  Bounds: array of integer;
  P: Pointer;
begin
  VarClear(Result);
  if Length(B) = 0 then
    Result := Null;

  SetLength(Bounds, 2);
  Bounds[0] := 0;
  Bounds[1] := Length(B) - 1;
  Result := VarArrayCreate(Bounds, varByte);
  if B <> nil then
  begin
    P := VarArrayLock(Result);
    Move(B[0], P^, Length(B));
    VarArrayUnlock(Result);
  end;
end;

class function TUtils.ConcatString(Strings: array of string; Separator,
  Pattern: string): string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Length(Strings) - 1 do
  begin
    if I > 0 then
      Result := Result + Separator;
    Result := Result + Format(Pattern, [Strings[I]]);
  end;
end;

class function TUtils.ConcatStrings(Strings: TList<string>; Separator,
  Pattern: string): string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Strings.Count - 1 do
  begin
    if I > 0 then
      Result := Result + Separator;
    Result := Result + Format(Pattern, [Strings[I]]);
  end;
end;

class function TUtils.Encode64(Data: TBytes): string;
const
  Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 0 to Length(Data) - 1 do
  begin
    x := Data[i];
    b := (b shl 8) or x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
  case Length(Data) mod 3 of
    1: Result := Result + '==';
    2: Result := Result + '=';
  end;
end;

class function TUtils.DateTimeToISO(DateValue: TDateTime): string;
var
  Fmt: TFormatSettings;
begin
  Fmt.ShortDateFormat := 'YYYY-mm-dd';
  Fmt.DateSeparator := '-';
  Fmt.ShortTimeFormat := 'hh:nn:ss.zzz';
  Fmt.TimeSeparator := ':';
  Fmt.DecimalSeparator := '.';
  Fmt.TimeAMString := 'AM';
  Fmt.TimePMString := 'PM';
  if DateValue = 0 then
    Result := ''
  else
  if DateOf(DateValue) = DateValue then
    Result := FormatDateTime('yyyy-mm-dd', DateValue, Fmt)
  else
  if TimeOf(DateValue) = DateValue then
    Result := FormatDateTime('hh:nn:ss.zzz', DateValue, Fmt)
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', DateValue, Fmt);
end;

class function TUtils.Decode64(S: string): TBytes;
const
  Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
var
  i: integer;
  a: integer;
  x: integer;
  b: integer;
  idx: integer;
  StrLen: integer;
  DataLen: integer;
begin
  StrLen := Length(S);
  DataLen := (StrLen shr 2) * 3;
  if S[StrLen] = '=' then
  begin
    DataLen := DataLen - 1;
    if S[StrLen - 1] = '=' then
      DataLen := DataLen - 1;
  end;
  SetLength(Result, DataLen);
  idx := 0;

  a := 0;
  b := 0;
  for i := 1 to StrLen do
  begin
    x := Pos(S[i], Codes64) - 1;
    if x >= 0 then
    begin
      b := (b shl 6) or x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x and 255;

        Assert(idx < DataLen);
        Result[idx] := x;
        idx := idx + 1;
      end;
    end
    else
      Exit;
  end;
end;

class function TUtils.GuidToVariant(G: TGuid): Variant;
begin
  if IsEqualGUID(EMPTY_GUID, G) then
    Result := Null
  else
  begin
    Result := GuidToString(G);
  end;
end;

class function TUtils.IntToAlias(Value: Integer): string;
begin
  Result := TUtils.IntToABC(Value);

  // To fix conflict with reserved words of SQL language
  // (I hope nobody reach these points - it would by ignorance - but the
  // framework has to support)

  if Result = 'AS' then
    Result := 'AS_';

  if Result = 'AT' then
    Result := 'AT_';

  if Result = 'BY' then
    Result := 'BY_';

  if Result = 'DO' then
    Result := 'DO_';

  if Result = 'IF' then
    Result := 'IF_';
end;

class function TUtils.IsNullGuid(G: TGuid): boolean;
begin
  Result := IsEqualGUID(EMPTY_GUID, G);
end;

class function TUtils.ISOToDateTime(const Value: string): TDateTime;
var
  Fmt: TFormatSettings;
begin
  if Value = '' then Exit(0);  
  Fmt.ShortDateFormat := 'YYYY-mm-dd';
  Fmt.DateSeparator := '-';
  Fmt.ShortTimeFormat := 'hh:nn:ss.zzz';
  Fmt.TimeSeparator := ':';
  Fmt.DecimalSeparator := '.';
  Fmt.TimeAMString := 'AM';
  Fmt.TimePMString := 'PM';
  Result := StrToDateTime(Value, Fmt);
end;

class function TUtils.NewGuid: TGuid;
begin
  {$IFDEF DELPHIXE_LVL}
  Result := TGuid.NewGuid;
  {$ELSE}
  if CreateGUID(Result) <> S_OK then
    RaiseLastOSError;
  {$ENDIF}
end;

class function TUtils.NullGuid: TGuid;
begin
  Result := EMPTY_GUID;
end;

class function TUtils.IntToABC(Value: Integer): string;
var
  D, M: Word;
begin
  DivMod(Value, 26, D, M);
  Result := Chr(Ord('A') + M);

  if (D > 0) then
    Result := IntToABC(D - 1) + Result;
end;

class function TUtils.VariantToBytes(V: Variant): TBytes;
var
  P: Pointer;
  L: Integer;
begin
  if VarIsClear(V) or VarIsNull(V) then
  begin
    SetLength(result, 0);
    Exit;
  end;

  if VarIsArray(v) then
  begin
    L := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    P := VarArrayLock(V);
    try
      SetLength(result, L);
      System.Move(P^, result[0], L);
    finally
      VarArrayUnlock(V);
    end;
  end
  else
  begin
    case VarType(V) of
      varString: result := BytesOf(string(V));
      varUString: result := BytesOf(string(V));
      varOleStr: result := WideBytesOf(UnicodeString(V));
    else
      // All other variant types are converted to string before storing.
      result := TEncoding.Unicode.GetBytes(VarToStr(V));
    end;
  end;
end;

class function TUtils.VariantToGuid(V: Variant): TGuid;
var
  S: string;
  B: TBytes;
begin
  if VarIsNull(V) or VarIsEmpty(V) then
    Exit(EMPTY_GUID);
  if VarIsArray(V) then
  begin
    B := VariantToBytes(V);
    {$IFDEF DELPHIXE_LVL}
    Result := TGuid.Create(B);
    {$ELSE}
    if Length(B) <> 16 then
      raise EArgumentException.Create('Byte array for GUID must be exactly 16 bytes long');
    Move(B[0], Result, SizeOf(Result));
    {$ENDIF}
  end else
  begin
    S := VarToStr(V);
    if S <> '' then
    begin
      if Length(S) = 36 then
        S := '{' + S + '}';
      Result := StringToGuid(S);
    end
    else
      Result := EMPTY_GUID;
  end;
end;

class function TUtils.VariantToString(Value: Variant): string;
var
  I: Integer;
begin
  if VarIsArray(Value) then
  begin
    Result := '';
    for I := 0 to VarArrayHighBound(Value, 1) do
    begin
      if I > 0 then
        Result := Result + ',';
      Result := Result + VarToStr(Value[I]);
    end;
  end
  else
    Result := VarToStr(Value);
end;

class function TUtils.VariantToValue(Value: Variant): TValue;
var
  DateTimeAux: TDateTime;
  BcdAux: string;
  Int64Aux:  Int64;
  IntAux: integer;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    Exit(TValue.Empty);

  // Check if the variant holds special data and do special treatment for it (datetime, blob, etc.)
  case VarTypeToDataType(VarType(Value)) of
    ftDateTime:
      begin
        // Workaround for converting TDateTime in Variant to TValue
        DateTimeAux := Value;
        TValue.Make(@DateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftTimeStamp:
      begin
        DateTimeAux := SQLTimeStampToDateTime(VarToSQLTimeStamp(Value));
        TValue.Make(@DateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftTimeStampOffset:
      begin
        DateTimeAux := SQLTimeStampOffsetToDateTime(VarToSQLTimeStampOffset(Value));
        TValue.Make(@DateTimeAux, TypeInfo(TDateTime), Result);
      end;
    ftFMTBCD, ftBCD:
      begin
        // Get the BCD
        BcdAux := BcdToStr(VarToBcd(Value));
        if TryStrToInt(BcdAux, IntAux) then
          Result := TValue.From<integer>(IntAux)
        else
        if TryStrToInt64(BcdAux, Int64Aux) then
          Result := TValue.From<Int64>(Int64Aux)
        else
          Result := TValue.From<double>(StrToFloat(BcdAux));
      end;
  else
    if ((VarType(Value) and varArray) = varArray) and ((VarType(Value) and varTypeMask) = varVariant) then
    begin
      // Composite id, set as variant
      TValue.Make(@Value, TypeInfo(Variant), Result);
    end else
      Result := TValue.FromVariant(Value);
  end;
end;

class function TUtils.ValueToVariant(Value: TValue): Variant;
var
  DateAux: TDateTime;
begin
  if Value.IsEmpty then
    Result := Variants.Null
  else
  if Value.TypeInfo = TypeInfo(Boolean) then
    Result := Value.AsBoolean
  else
  if (Value.TypeInfo = TypeInfo(TDateTime))
    or (Value.TypeInfo = TypeInfo(TDate))
    or (Value.TypeInfo = TypeInfo(TTime)) then
  begin
    // Force the variant result to be date time
    // It was returning as double, and it was causing problems in some specific databases
    // ElevateDB was raising a conversion error when setting a ftDate param but setting the value as variant
    // Same for TTime and TDateTime
    DateAux := TDateTime(Value.AsType<double>);
    Result := DateAux;
  end
  else
  if (Value.TypeInfo = TypeInfo(TBcd)) then
  begin
    Result := BcdToStr(Value.AsType<TBcd>);
  end
  else
  if (Value.TypeInfo = TypeInfo(TGuid)) then
  begin
    Result := GuidToVariant(Value.AsType<TGuid>);
  end
  else
    Result := Value.AsVariant;
end;

end.
