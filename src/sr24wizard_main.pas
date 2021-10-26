unit SR24wizard_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, Menus, ActnList, ExtCtrls, lclintf, XMLPropStorage, SR24_chsets,
  SR24_dec;

type                                                   {Definitions for TForm1}

  { TForm1 }

  TForm1 = class(TForm)
    actClose: TAction;
    actBind: TAction;
    actFindInfo: TAction;
    actAbout: TAction;
    actHomepage: TAction;
    actManual: TAction;
    actOpen: TAction;
    actDefault: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    ActionList: TActionList;
    btnSave: TBitBtn;
    btnDefault: TBitBtn;
    btnInfo: TBitBtn;
    btnClose: TBitBtn;
    btnClose1: TBitBtn;
    btnLoad: TBitBtn;
    btnBind: TBitBtn;
    cbxPort: TComboBox;
    ImageList: TImageList;
    lblSerial: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    N4: TMenuItem;
    mnHomepage: TMenuItem;
    N3: TMenuItem;
    mnAbout: TMenuItem;
    mnManual: TMenuItem;
    mnFindInfo: TMenuItem;
    mnBind: TMenuItem;
    mnInfo: TMenuItem;
    mnTools: TMenuItem;
    N2: TMenuItem;
    mnClose: TMenuItem;
    mnSaveAs: TMenuItem;
    N1: TMenuItem;
    mnSave: TMenuItem;
    mnDefault: TMenuItem;
    mnOpen: TMenuItem;
    mmoText: TMemo;
    mmoInfo: TMemo;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog: TSaveDialog;
    tabTools: TTabSheet;
    tabTextedit: TTabSheet;
    tmrBind: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure actAboutExecute(Sender: TObject);
    procedure actBindExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDefaultExecute(Sender: TObject);
    procedure actFindInfoExecute(Sender: TObject);
    procedure actHomepageExecute(Sender: TObject);
    procedure actManualExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure cbxPortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrBindTimer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  SR24Connected: boolean;

const
  infofile='RCsettings_info.txt';
  manual='SR24Settings.pdf';
  hppdf='/pdf/';
  meinname='Helmut Elsner';
  email   ='helmut.elsner@live.com';                   {My e-mail address}
  homepage='http://h-elsner.mooo.com';                 {My Homepage}
  githublink='https://github.com/h-elsner/Q500log2kml';
  capForm1='Edit settings for RC models with SR24';
  UARTspeed=115200;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:=capForm1;
  SR24connected:=false;
  mmoText.Lines.Clear;
  mmoInfo.Text:='Read manual.';
{$IFDEF WINDOWS}
  cbxPort.ItemIndex:=3;
{$ELSE}
  cbxPort.ItemIndex:=0;
{$EndIf}

  if FileExists(Application.Location+filename_settings) then
    mmoText.Lines.LoadFromFile(Application.Location+filename_settings);
  if FileExists(Application.Location+infofile) then begin
    mmoInfo.Lines.LoadFromFile(Application.Location+infofile);
    actFindInfo.Enabled:=false;
  end;
  OpenDialog.FileName:=filename_settings;
  SaveDialog.FileName:=filename_settings;
  OpenDialog.InitialDir:=Application.Location;
  SaveDialog.InitialDir:=Application.Location;
  actSave.Hint:='Save settings file as "'+filename_settings+'".';
end;

procedure TForm1.tmrBindTimer(Sender: TObject);        {sends 5 times bind message}
begin
  tmrBind.Enabled:=false;
  if SR24connected then begin
    SendBind;
    tmrBind.Tag:=tmrBind.Tag+1;
    if tmrBind.Tag>5 then begin
      tmrBind.Tag:=1;
      DisconnectUART(SR24connected);
      btnClose.Enabled:=true;
      btnClose1.Enabled:=true;
    end else
      tmrBind.Enabled:=true;
  end;
end;

procedure TForm1.actCloseExecute(Sender: TObject);
begin
  if SR24connected then
    DisconnectUART(SR24connected);
  Close;
end;

procedure TForm1.actBindExecute(Sender: TObject);      {Send Bind message}
begin
  tmrBind.Tag:=1;
  if SR24connected then begin
    btnClose.Enabled:=false;
    btnClose1.Enabled:=false;
    ConnectUART(cbxPort.Text, UARTspeed, SR24connected);
    tmrBind.Enabled:=true;
  end else begin
    btnBind.Enabled:=false;                            {no connection}

  end;
end;

function OpenManual: boolean;                          {Handbuch aufrufen}
begin
  if not FileExists(Application.Location+manual) then
    result:=OpenURL(homepage+hppdf+manual)
  else
    result:=OpenDocument(Application.Location+manual);
end;

procedure TForm1.actAboutExecute(Sender: TObject);     {About box}
begin
  if MessageDlg(capForm1+sLineBreak+sLineBreak+meinname+sLineBreak+
                homepage+sLineBreak+email,
                mtInformation,[mbHelp, mbOK],0)=mrNone then
  OpenManual;
end;

procedure TForm1.actDefaultExecute(Sender: TObject);   {Load defaults}
var
  list: TStringList;
  i: integer;

begin
  mmoText.Lines.Clear;
  list:=TStringList.Create;
  try
    mmoText.Lines.Add('# Default settings');
    mmoText.Lines.Add('');
    SettingsToText(DefaultSettings, list);
    for i:=0 to list.Count-1 do
      mmoText.Lines.Add(list[i]);
  finally
    list.Free;
  end;
end;

procedure TForm1.actFindInfoExecute(Sender: TObject);  {Find info}
begin
  if FileExists(Application.Location+infofile) then
    mmoInfo.Lines.LoadFromFile(Application.Location+infofile)
  else
    OpenManual;
end;

procedure TForm1.actHomepageExecute(Sender: TObject);  {Homepage}
begin
  OpenURL(homepage);
end;

procedure TForm1.actManualExecute(Sender: TObject);    {Load pdf manual}
begin
  OpenManual;
end;

procedure TForm1.actOpenExecute(Sender: TObject);      {Open settings file}
begin
  if Opendialog.Execute then
    mmoText.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TForm1.actSaveAsExecute(Sender: TObject);    {Save as}
begin
  if SaveDialog.Execute then
    mmoText.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TForm1.actSaveExecute(Sender: TObject);      {Save settings}
begin
  mmoText.Lines.SaveToFile(OpenDialog.FileName);
end;

procedure TForm1.cbxPortChange(Sender: TObject);       {select UART port}
begin
  btnBind.Enabled:=true;
end;

end.

