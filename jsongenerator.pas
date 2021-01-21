unit JSONGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GenerateJSON(const ATemplate: String; ACurrentPath: TFileName; AStream: TStream);

implementation

uses Variants, SynCommons, Math, FileUtil;

type
  TMethodKind = (mkNone, mkUnknown, mkInclude, mkRandom, mkLoop, mkSetVar, mkGetVar, mkFormat, mkFormatNumber);

var
  MethodNames: array[TMethodKind] of String;

type
  { TJSONGenerator }

  TJSONGenerator = class(TTextWriter)
  private
    CurrentPath: TFileName;
    Escape: TTextWriterKind;
    LocalVars: array of record
      Name: RawUTF8;
      Value: Variant;
    end;
    function GetMethodKind(const AName: RawUTF8): TMethodKind;
    function AddLocalVar(const AName: RawUTF8): Integer;
    procedure RemoveLocalVar(const AIndex: Integer);
    procedure SetLocalVar(const AIndex: Integer; const AValue: Variant);
    function GetLocalVar(const AName: RawUTF8; out AValue: Variant): Boolean;
    function GetRandomValue(const AValue: TDocVariantData): Variant;
    function GetValue(const AValue: Variant): Variant;
    function DoValue(const AValue: Variant; const AName: RawUTF8 = ''): TMethodKind;
    procedure DoObject(const AValue: TDocVariantData);
    procedure DoArray(const AValue: TDocVariantData);
    procedure DoMehtod(AKind: TMethodKind; const AValue: Variant);
    procedure DoInclude(const AValue: Variant);
    procedure DoRandom(const AValue: TDocVariantData);
    procedure DoLoop(const AValue: TDocVariantData);
    procedure DoGetVar(const AValue: Variant);
    procedure DoSetVar(const AValue: TDocVariantData);
    procedure DoFormat(const AValue: TDocVariantData);
    procedure DoFormatNumber(const AValue: TDocVariantData);
  public
    constructor Create(const ATemplate: RawUTF8; ACurrentPath: TFileName; AStream: TStream); reintroduce;
    destructor Destroy; override;
  end;

procedure GenerateJSON(const ATemplate: String; ACurrentPath: TFileName; AStream: TStream);
begin
  TJSONGenerator.Create(ATemplate, ACurrentPath, AStream).Free;
end;

{ TJSONGenerator }

function TJSONGenerator.GetMethodKind(const AName: RawUTF8): TMethodKind;
var
  mk: TMethodKind;
  N: RawUTF8;
begin
  if (Length(AName) >= 2) and (AName[1] = '$') then
  begin
    Result := mkUnknown;
    N := LowerCase(AName);
    for mk := Low(TMethodKind) to High(TMethodKind) do
      if MethodNames[mk] = N then
      begin
        Result := mk;
        Break;
      end;
  end
  else
    Result := mkNone;
end;

function TJSONGenerator.AddLocalVar(const AName: RawUTF8): Integer;
var
  i: Integer;
begin
  if AName = '' then
    Exit(-1);
  for i := 0 to High(LocalVars) do
    with LocalVars[i] do
      if Name = AName then
        Exit(i);
  SetLength(LocalVars, Length(LocalVars) + 1);
  Result := High(LocalVars);
  LocalVars[Result].Name := AName;
end;

procedure TJSONGenerator.RemoveLocalVar(const AIndex: Integer);
begin
  if AIndex = -1 then
    Exit;
  Delete(LocalVars, AIndex, 1);
end;

procedure TJSONGenerator.SetLocalVar(const AIndex: Integer; const AValue: Variant);
begin
  LocalVars[AIndex].Value := AValue;
end;

function TJSONGenerator.GetLocalVar(const AName: RawUTF8; out AValue: Variant): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(LocalVars) do
    with LocalVars[i] do
      if Name = AName then
      begin
        AValue := Value;
        Result := True;
        Break;
      end;
end;

function TJSONGenerator.GetRandomValue(const AValue: TDocVariantData): Variant;
var
  VV: Variant;
  DV: TDocVariantData;
  mn, mx: Double;
begin
  Result := nil;
  with AValue do
    case Kind of
      dvObject:
      begin
        if GetValueIndex('$GetVar') = -1 then
        begin
          mn := D['$Min'];
          mx := D['$Max'];
          if (Frac(mn) = 0) and (Frac(mx) = 0) then
            Result := RandomRange(Trunc(mn), trunc(mx))
          else
            Result := mn + Random * (mx - mn);
        end
        else if GetLocalVar(S['$GetVar'], VV) then
        begin
          if DocVariantType.IsOfType(VV) then
          begin
            DV := _Safe(GetValue(VV))^;
            case DV.Kind of
              dvObject: Result := GetValue(VV);
              dvArray: Result := GetRandomValue(DV);
            end;
          end;
        end;
      end;
      dvArray: Result := Values[Random(Count)];
    end;
end;

function TJSONGenerator.GetValue(const AValue: Variant): Variant;
var
  DV: TDocVariantData;
  MK: TMethodKind;
begin
  Result := AValue;
  if DocVariantType.IsOfType(AValue) then
  begin
    DV := _Safe(AValue)^;
    with DV do
      case Kind of
        dvObject:
        begin
          if Count = 1 then
          begin
            MK := GetMethodKind(Names[0]);
            case MK of
              mkRandom: Result := GetRandomValue(TDocVariantData(Values[0]));
              mkGetVar: GetLocalVar(VarToStr(DV.S['$GetVar']), Result);
            end;
          end;
        end;
      end;
  end;
end;

function TJSONGenerator.DoValue(const AValue: Variant; const AName: RawUTF8): TMethodKind;
var
  V: String;
  DV: TDocVariantData;
begin
  Result := mkNone;
  if DocVariantType.IsOfType(AValue) then
  begin
    DV := _Safe(AValue)^;
    with DV do
      case Kind of
        dvObject:
        begin
          if Count = 1 then
          begin
            Result := GetMethodKind(Names[0]);
            if not (Result in [mkSetVar, mkInclude]) then
              if AName <> '' then
              begin
                AddJSONString(AName);
                Add(':');
              end;
            case Result of
              mkNone, mkUnknown: DoObject(DV);
              else
              begin
                DoMehtod(Result, Values[0]);
                Exit;
              end;
            end;
          end
          else
          begin
            if AName <> '' then
            begin
              AddJSONString(AName);
              Add(':');
            end;
            DoObject(DV);
          end;
        end;
        dvArray:
        begin
          AddJSONString(AName);
          Add(':');
          DoArray(DV);
        end;
      end;
  end
  else
  begin
    if AName <> '' then
    begin
      AddJSONString(AName);
      Add(':');
    end;
    if VarIsStr(AValue) then
    begin
      V := VarToStr(AValue);
      Result := GetMethodKind(V);
      case Result of
        mkNone, mkUnknown: ;
        else
        begin
          DoMehtod(Result, nil);
          Exit;
        end;
      end;
    end;
    AddVariant(AValue, Escape);
  end;
end;

procedure TJSONGenerator.DoObject(const AValue: TDocVariantData);
var
  j: Integer;
begin
  Add('{');
  with AValue do
    for j := 0 to Count - 1 do
    begin
      if not (DoValue(Values[j], Names[j]) in [mkSetVar, mkInclude]) then
        Add(',');
    end;
  CancelLastComma;
  Add('}');
end;

procedure TJSONGenerator.DoArray(const AValue: TDocVariantData);
var
  j: Integer;
begin
  Add('[');
  with AValue do
    for j := 0 to Count - 1 do
    begin
      DoValue(Values[j]);
      Add(',');
    end;
  CancelLastComma;
  Add(']');
end;

procedure TJSONGenerator.DoMehtod(AKind: TMethodKind; const AValue: Variant);
begin
  case AKind of
    mkNone: ;
    mkUnknown: ;
    mkInclude: DoInclude(AValue);
    mkRandom: DoRandom(TDocVariantData(AValue));
    mkLoop: DoLoop(TDocVariantData(AValue));
    mkSetVar: DoSetVar(TDocVariantData(AValue));
    mkGetVar: DoGetVar(AValue);
    mkFormat: DoFormat(TDocVariantData(AValue));
    mkFormatNumber: DoFormatNumber(TDocVariantData(AValue));
  end;
end;

procedure TJSONGenerator.DoInclude(const AValue: Variant);
var
  N, FN: TFileName;
  T: RawUTF8;
  Value: Variant;
  j: Integer;
begin
  FN := '';
  N := VarToStr(AValue);
  if FileExists(N) then
    FN := VarToStr(AValue)
  else if FileExists(CurrentPath + N) then
    FN := CurrentPath + N;
  if FN = '' then
    Exit;
  T := StringFromFile(FN);
  if TDocVariantData(Value).InitJSONInPlace(Pointer(T), [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]) <> nil then
    with TDocVariantData(Value) do
      for j := 0 to Count - 1 do
        DoValue(Values[j]);
end;

procedure TJSONGenerator.DoRandom(const AValue: TDocVariantData);
var
  CI, i, CC: Int64;
begin
  case AValue.Kind of
    dvObject:
    begin
      CI := AValue.GetValueIndex('$Count');
      if CI = -1 then
        DoValue(GetRandomValue(AValue))
      else
      begin
        CC := 0;
        VariantToInt64(GetValue(AValue.Value[CI]), CC);
        Add('[');
        for i := 0 to CC - 1 do
        begin
          DoValue(GetRandomValue(AValue));
          Add(',');
        end;
        CancelLastComma;
        Add(']');
      end;
    end;
    dvArray: DoValue(GetRandomValue(AValue));
  end;
end;

procedure TJSONGenerator.DoLoop(const AValue: TDocVariantData);
var
  v, Mn, Mx, Stp: Int64;
  BI, Lvi: Integer;
  Blc: Variant;
  Vr: String;
begin
  with AValue do
    if Kind = dvObject then
    begin
      BI := GetValueIndex('$Block');
      if BI = -1 then
        Exit;
      Blc := Values[BI];
      Mn := I['$From'];
      Mx := I['$To'];
      Stp := I['$Step'];
      if Stp = 0 then
        Stp := 1;
      Vr := S['$Var'];
      Lvi := AddLocalVar(Vr);
      Add('[');
      v := Mn;
      while v <= Mx do
      begin
        if Lvi <> -1 then
          SetLocalVar(Lvi, v);
        DoValue(Blc);
        Add(',');
        Inc(v, Stp);
      end;
      CancelLastComma;
      Add(']');
      RemoveLocalVar(Lvi);
    end;
end;

procedure TJSONGenerator.DoGetVar(const AValue: Variant);
var
  V: Variant;
begin
  if GetLocalVar(VarToStr(AValue), V) then
    DoValue(V);
end;

procedure TJSONGenerator.DoSetVar(const AValue: TDocVariantData);
var
  Nm: String;
  Vr: Variant;
  VI: Integer;
begin
  with AValue do
    if Kind = dvObject then
    begin
      Nm := S['$Name'];
      Vr := Value['$Value'];
      Vr := Variant(_Safe(Vr)^);
      VI := AddLocalVar(Nm);
      SetLocalVar(VI, Vr);
    end;
end;

procedure TJSONGenerator.DoFormat(const AValue: TDocVariantData);
var
  Vr: PDocVariantData;
  Tmp: RawUTF8;
  F: AnsiChar;
  AC, TI: Integer;
begin
  with AValue do
    if Kind = dvObject then
    begin
      Vr := A['$Value'];
      if Vr^.Kind <> dvArray then
        Exit;
      TI := GetValueIndex('$Template');
      if TI < 0 then
        Exit;
      Tmp := VarToStr(GetValue(Values[Ti]));
      Add('"');
      AC := 0;
      for F in Tmp do
        if (F <> '?') or (AC = Vr^.Count) then
          Add(F)
        else
        begin
          Escape := twNone;
          DoValue(Vr^.Values[AC]);
          Escape := twJSONEscape;
          AC += 1;
        end;
      Add('"');
    end;
end;

procedure TJSONGenerator.DoFormatNumber(const AValue: TDocVariantData);
var
  Val: PDocVariantData;
  V: Double;
  Tmp: RawUTF8;
begin
  with AValue do
    if Kind = dvObject then
    begin
      Val := O['$Value'];
      Tmp := VarToStr(GetValue(Value['$Template']));
      case Val^.Kind of
        dvObject: AddNoJSONEscapeString(FormatFloat(Tmp, VariantToDoubleDef(GetValue(Value['$Value']), 0)));
        dvUndefined: if GetAsDouble('$Value', V) then
            AddNoJSONEscapeString(FormatFloat(Tmp, V));
      end;
    end;
end;

constructor TJSONGenerator.Create(const ATemplate: RawUTF8; ACurrentPath: TFileName; AStream: TStream);
var
  Value: Variant;
begin
  inherited Create(AStream);
  CurrentPath := ACurrentPath;
  Escape := twJSONEscape;
  if TDocVariantData(Value).InitJSONInPlace(Pointer(ATemplate), [dvoReturnNullForUnknownProperty, dvoValueCopiedByReference]) <> nil then
    DoValue(Value)
  else
    VarClear(Value);
end;

destructor TJSONGenerator.Destroy;
begin
  FlushFinal;
  inherited Destroy;
end;

procedure PrepareMethodNames;
var
  mk: TMethodKind;
begin
  for mk := Low(TMethodKind) to High(TMethodKind) do
    MethodNames[mk] := '$' + LowerCase(TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TMethodKind), Integer(mk))));
end;

initialization
  PrepareMethodNames;
end.
