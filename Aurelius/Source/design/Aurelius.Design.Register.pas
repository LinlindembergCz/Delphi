unit Aurelius.Design.Register;

interface

procedure Register;

implementation
uses
  Windows, SysUtils, Classes, DB, DesignIntf, ToolsApi,
  Aurelius.Bind.Dataset,
  Aurelius.Design.DatasetEditor,
  Aurelius.Design.DatasetDesigner;

procedure Register;
begin
  ForceDemandLoadState(dlDisable); // Aurelius is very lightweight package, no problem. Change this if it becomes "heavy"
  RegisterComponents('TMS Aurelius', [TAureliusDataset]);
  RegisterComponentEditor(TAureliusDataset, TAureliusDataSetEditor);
  RegisterFields([TAureliusEntityField]);
end;

const
  AureliusVersion = '2.2';

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  BitmapHandle: HBITMAP;
begin
  Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices);
  Assert(AboutBoxServices <> nil);
  BitmapHandle := LoadBitmap(FindResourceHInstance(HInstance), 'AURELIUSSPLASH');
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(
    'TMS Aurelius ' + AureliusVersion,
    'TMS Aurelius ' + AureliusVersion + #13#10 +
    'Object-Relational Framework for Delphi'#13#10 +
    '(c) 2012 tmssoftware.com bvba'#13#10 +
    'All Rights Reserved.',
    BitmapHandle,
    {$IFDEF TRIAL}
    True, 'Trial version'
    {$ELSE}
    False, 'Registered version'
    {$ENDIF}
  );
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and (AboutBoxServices <> nil) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  BitmapHandle: HBITMAP;
begin
  Assert(SplashScreenServices <> nil);
  BitmapHandle := LoadBitmap(FindResourceHInstance(HInstance), 'AURELIUSSPLASH');

  SplashScreenServices.AddPluginBitmap('TMS Aurelius ' + AureliusVersion,
    BitmapHandle,
    {$IFDEF TRIAL}
    True, 'Trial version'
    {$ELSE}
    False, 'Registered version'
    {$ENDIF}
  );
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnregisterAboutBox;

end.
