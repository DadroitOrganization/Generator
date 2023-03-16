program JSONGeneratorCLI;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  FileInfo,
  CustApp,
  mormot.core.os,
  mormot.core.base,
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
    OutName: string;
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
    Message: string;
    version: TProgramVersion;
  begin
    Message := ExeVersion.ProgramName;
    WriteLn(#13#10 + Message);
    Message := StringOfChar('-', Length(Message));
    WriteLn(Message);
    FileInfo.GetProgramVersion(version);
    WriteLn(Format('%d.%d.%d.%d', [version.Major, version.Minor,
      version.Revision, version.Build]));
    WriteLn;
    WriteLn(Format('Usage: %s template.json output.json', [ExeVersion.ProgramName]));
    WriteLn;
  end;

  constructor TJSONGeneratorCLI.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  function GetVendorName: string;
  begin
    Result := 'Dadroit';
  end;

  function GetApplicationName: string;
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
  Application.Title := 'JSON Generator';
  Application.Run;
  Application.Free;
end.
