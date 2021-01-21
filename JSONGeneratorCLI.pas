program JSONGeneratorCLI;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
 {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  SynCommons,
  JSONGenerator;

type

  { TJSONGeneratorCLI }

  TJSONGeneratorCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoHelp;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TJSONGeneratorCLI }

  procedure TJSONGeneratorCLI.DoRun;
  var
    OutName: String;
    OutStr: TFileStream;
  begin
    if HasOption('h', 'help') or (ParamCount < 1) then
    begin
      DoHelp;
      Terminate;
      Exit;
    end;
    if not FileExists(ParamStr(1)) then
    begin
      WriteLn('File not found.');
      Terminate;
      Exit;
    end;
    Randomize;
    try
      if ParamCount < 2 then
        OutName := ChangeFileExt(ParamStr(1), '.out.json')
      else
        OutName := ParamStr(2);
      OutStr := TFileStream.Create(OutName, fmCreate or fmShareDenyWrite);
      GenerateJSON(StringFromFile(ParamStr(1)), ExtractFilePath(ParamStr(1)), OutStr);
    finally
      OutStr.Free;
    end;
    Terminate;
  end;

  procedure TJSONGeneratorCLI.DoHelp;
  var
    Message: String;
  begin
    Message := ExeVersion.ProgramName;
    WriteLn(#13#10 + Message);
    Message := StringOfChar('-', Length(Message));
    WriteLn(Message);
    WriteLn(ExeVersion.Version.Detailed);
    WriteLn;
    WriteLn(Format('usage: %s template.json output.json', [ExeVersion.ProgramName]));
    WriteLn;
  end;

  constructor TJSONGeneratorCLI.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  function GetVendorName: String;
  begin
    Result := 'Dadroit';
  end;

  function GetApplicationName: String;
  begin
    Result := 'JSONGenerator';
  end;

var
  Application: TJSONGeneratorCLI;

{$R *.res}

begin
  OnGetApplicationName := @GetApplicationName;
  OnGetVendorName := @GetVendorName;
  Application := TJSONGeneratorCLI.Create(nil);
  Application.Name := ApplicationName;
  Application.Title := VendorName + ' ' + ApplicationName;
  Application.Run;
  Application.Free;
end.
