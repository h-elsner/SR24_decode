unit SR24wizard_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, Menus, ActnList, ExtCtrls, lclintf, XMLPropStorage, process,
  SR24_chsets, SR24_dec;

type                                                   {Definitions for TForm1}

  { TForm1 }

  TForm1 = class(TForm)
    actClose: TAction;
    actBind: TAction;
    actFindInfo: TAction;
    actAbout: TAction;
    actHomepage: TAction;
    actCheck: TAction;
    actManual: TAction;
    actOpen: TAction;
    actDefault: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    ActionList: TActionList;
    btnCheck: TBitBtn;
    btnSave: TBitBtn;
    btnDefault: TBitBtn;
    btnInfo: TBitBtn;
    btnClose: TBitBtn;
    btnClose1: TBitBtn;
    btnLoad: TBitBtn;
    btnBind: TBitBtn;
    cbxPort: TComboBox;
    ImageList: TImageList;
    lblTemp: TLabel;
    lblSerial: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    N6: TMenuItem;
    MenuItem3: TMenuItem;
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
    tmrTemp: TTimer;
    tmrBind: TTimer;
    XMLPropStorage1: TXMLPropStorage;
    procedure actAboutExecute(Sender: TObject);
    procedure actBindExecute(Sender: TObject);
    procedure actCheckExecute(Sender: TObject);
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
    procedure tmrTempTimer(Sender: TObject);
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

  rsBind1='LED at SR24 slow binking: Waiting for connection.';
  rsBind2='LED at SR24 solid:        Connected to RC.';
  rsBind3='>>>>> Sending BIND command...';
  rsBind4='LED at SR24 fast binking: Bind mode.';
  rsHint='Hint:    ';
  rsWarn='Warning: ';
  rsErr= 'Error:   ';
  rsSave='Save settings file as';
  rsDefault='# Default settings';
  rsCheck='Checking current settings file in text editor.';

  errNoCon='SR24 not connected';
  errLoad='Load settings file to check.';
  errInvPio='Invalid GPIO pin number.';
  rsValidNR='Valid but not recommended GPIO pin.';
  rsWarnPWM='PWM channels should not assigned to switches.';
  rsGPIO='GPIO';
  rsNoPins='No GPIO pins assigned.';
  rsNoPWM='No PWM channels assigned';
  errPinX=' already used. Do you really want to use this pin for multiple purposes?';
  errChanX=' already used. Do you really want to use this channel for multiple purposes?';
  errInvChan='Invalid RC channel number.';
  errPWMset='PWM channel must not assigned here.';
  rsCheckOK='Settings seems to be OK. No inconsistency found.';
  rsDefaultSet='OK, those are the default settings.';
  rsLines='A settings file should have more than one line. No check done.';
  tab1=' ';

  servx=[1..4, 7, 8];                                  {Valid channel numbers for servos}
  switx=[5, 6, 9..12];                                 {Valid channel numbers for switches (6 usually not used)}
  recGPIO=[5, 6, 12, 16, 17, 22..27];                  {Recommended GPIO numbers}


implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:=capForm1;
  SR24connected:=false;
  mmoText.Lines.Clear;
  mmoInfo.Text:='Info text';
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
  actSave.Hint:=rsSave+' "'+filename_settings+'".';
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

procedure TForm1.tmrTempTimer(Sender: TObject);       {Raspi temperature check}
var
  s: string;

begin
  RunCommand('vcgencmd', ['measure_temp'], s);
  if trim(s)<>'' then
    lblTemp.Caption:='SoC '+s;
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
  mmoInfo.Lines.Clear;
  ConnectUART(cbxPort.Text, UARTspeed, SR24connected);
  if UARTCanWrite then begin
    btnClose.Enabled:=false;
    btnClose1.Enabled:=false;
    mmoInfo.Lines.Add(rsBind1);
    mmoInfo.Lines.Add(rsBind2);
    mmoInfo.Lines.Add(rsBind3);
    mmoInfo.Lines.Add(rsBind4);
    tmrBind.Enabled:=true;
  end else begin
    btnBind.Enabled:=false;                            {no connection}
    mmoInfo.Lines.Add(errNoCon);
  end;
end;

function ValidGPIO(GPIOnr: byte): byte;                {0..4 kind of pin number}
begin
  result:=0;                                           {Invalid GPIO pin}
  if (GPIOnr=0) or (GPIOnr=1) then begin
    result:=3;                                         {2 HW PWN channels (0, 1)}
    exit;
  end;
  if GPIOnr=notused then begin
    result:=4;                                         {GPIOnr 88 - not used}
    exit;
  end;
  if (GPIOnr>1) and (GPIOnr<28) then begin
    result:=1;                                         {Valid GPIO pin number}
    if GPIOnr in recGPIO then
      result:=2;                                       {Recommended GPIO pin}
  end;
end;

function CompareToDefault(csets: TSettings):boolean;   {Check against default}
var
  i, k: byte;

begin
  result:=true;
  for i:=0 to 12 do
    for k:=0 to 5 do
      if csets[i, k]<>DefaultSettings[i, k] then begin
        result:=false;
        exit;
      end;
end;

procedure TForm1.actCheckExecute(Sender: TObject);     {Check consistency of settings}
var
  csets: TSettings;
  inlist: TStringList;
  i, zhl, g: integer;
  k: byte;
  ps, ps2, cs: string;
  charr: array[1..12] of byte;                         {Counter used channels}
  pioarr: array[0..27] of byte;                        {Counter used GPIO pins}

  function CheckGPIO(idx, spx: byte; pwm: boolean=true): string;
  var
    kp: byte;

  begin
    result:='';                                        {Nothing to worry about}
    kp:=ValidGPIO(csets[idx, spx]);
    case kp of
      0: result:=result+rsErr+errInvPio+LineEnding;
      1: begin
           result:=result+rsHint+rsValidNR+LineEnding;
           pioarr[csets[idx, spx]]:=pioarr[csets[idx, spx]]+1;
           if pioarr[csets[idx, spx]]>1 then
             result:=result+rsWarn+rsGPIO+tab1+IntToStr(csets[idx, spx])+errPinX+LineEnding;
         end;
      2: begin
           pioarr[csets[idx, spx]]:=pioarr[csets[idx, spx]]+1;
           if pioarr[csets[idx, spx]]>1 then
             result:=result+rsWarn+rsGPIO+tab1+IntToStr(csets[idx, spx])+errPinX+LineEnding;
         end;
      3: if pwm then begin
           pioarr[csets[idx, spx]]:=pioarr[csets[idx, spx]]+1;
           if idx>6 then
             result:=result+rsWarn+rsWarnPWM+LineEnding;
           if pioarr[csets[idx, spx]]>1 then
             result:=result+rsWarn+'PWM'+IntToStr(csets[idx, spx])+errPinX+LineEnding;
         end else begin
           pioarr[csets[idx, spx]]:=pioarr[csets[idx, spx]]+1;
           result:=result+rsErr+errPWMset+LineEnding;
         end;
    end;
  end;

  function CheckChannel(idx, spx: byte; mix: boolean=false): string;
  begin
    result:='';                                        {Nothing to worry about}
    if (csets[idx, spx]>0) and (csets[idx, spx]<13) then begin   {channel number must be 1-12}
      charr[csets[idx, spx]]:=charr[csets[idx, spx]]+1;
      if (not mix) and (charr[csets[idx, spx]]>1) then
        result:=result+rsWarn+chnr+tab1+IntToStr(csets[idx, spx])+errPinX+LineEnding;
      if (idx>6) and (idx<12) and
         (csets[idx, spx] in servx) then               {Proper channels for switches}
        result:=result+rsHint+'In ST16 standard settings those channels belongs to servos.'+LineEnding;
      if (idx<7) and (csets[idx, spx] in switx) then   {Proper channels for servos}
        result:=result+rsHint+'In ST16 standard settings those channels belongs to switches and buttons.'+LineEnding;
    end else
      result:=result+rsErr+errInvChan+LineEnding;      {Invalid channel number}
  end;

begin
  mmoInfo.Lines.Clear;
  zhl:=0;
  g:=0;
  if mmoText.Lines.Count>5 then begin
    mmoInfo.Lines.Add(rsCheck);
    mmoInfo.Lines.Add('');
    inlist:=TStringList.Create;
    Screen.Cursor:=crHourGlass;
    for i:=1 to 12 do
      charr[i]:=0;
    for i:=0 to 27 do
      pioarr[i]:=0;
    try
      inlist.Assign(mmoText.Lines);
      if inlist.Count>1 then begin
        csets:=ReadSettingsList(inlist);
        if CompareToDefault(csets) then begin
          mmoInfo.Lines.Add(rsDefaultSet)
        end else begin
          ps:=CheckGPIO(0, 1, false);                  {Voltage warning 1}
          if ps<>'' then begin
            mmoInfo.Lines.Add(warn1);
            mmoInfo.Lines.Add(ps);
            inc(zhl);
          end;
          ps:=CheckGPIO(0, 2, false);                  {Voltage warning 2}
          if ps<>'' then begin
            mmoInfo.Lines.Add(warn2);
            mmoInfo.Lines.Add(ps);
            inc(zhl);
          end;

          for i:=1 to 6 do begin                       {Check settings for servos}
            cs:=CheckChannel(i, 0, false);
            ps:=CheckGPIO(i, 4, true);
            ps2:='';
            k:=ValidGPIO(csets[i, 4]);
            case k of                                  {Check GPIOnum 2 depending on GPIO assigned}
              1, 2: ps2:=CheckGPIO(i, 5, false);
              3: if (csets[i, 5]>1) then
                   ps2:=rsErr+pio2+' must not assigned to PWM channels. Use default 0 or 1 for PWM reversed.';
            end;
            if (cs<>'') or (ps<>'') or (ps2<>'') then begin
              mmoInfo.Lines.Add(svx+tab1+IntToStr(i));
              if cs<>'' then
                mmoInfo.Lines.Add(cs);
              if ps<>'' then
                mmoInfo.Lines.Add(ps);
              if ps2<>'' then
                mmoInfo.Lines.Add(ps2);
              inc(zhl);
            end;
          end;

          for i:=7 to 11 do begin                      {Check settings for switches}
            cs:=CheckChannel(i, 0);
            ps:=CheckGPIO(i, 4, false);
            ps2:=CheckGPIO(i, 5, false);               {GPIOnum 2}
            if (cs<>'') or (ps<>'') or (ps2<>'') then begin
              mmoInfo.Lines.Add(swx+tab1+IntToStr(i-6));
              if cs<>'' then
                mmoInfo.Lines.Add(cs);
              if ps<>'' then
                mmoInfo.Lines.Add(ps);
              if ps2<>'' then
                mmoInfo.Lines.Add(ps2);
              inc(zhl);
            end;
          end;

          ps:=CheckGPIO(12, 2, false);                 {Start/stop}
          cs:=CheckChannel(12, 0, true);
          if (ps<>'') or (cs<>'') then begin
            mmoInfo.Lines.Add(abtn);
            if cs<>'' then
              mmoInfo.Lines.Add(cs);
            if ps<>'' then
              mmoInfo.Lines.Add(ps);
            inc(zhl);
          end;

          for i:=2 to 27 do
            g:=g+pioarr[i];                            {Number of assigned GPIO pins}
          if g=0 then begin
            mmoInfo.Lines.Add(rsHint+rsNoPins);
            inc(zhl);
          end;
          if (pioarr[0]+pioarr[1])=0 then begin        {Number of assigned PWM channels}
            mmoInfo.Lines.Add(rsHint+rsNoPWM);
            inc(zhl);
          end;

          if zhl=0 then
            mmoInfo.Lines.Add(rsCheckOK);
        end;
      end else
        mmoInfo.Lines.Add(rsLines);                    {Not enough lines in file}
    finally
      inlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end else
    mmoInfo.Lines.Add(errLoad);
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
    mmoText.Lines.Add(rsDefault);
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
  if OpenDialog.Execute then
    mmoText.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TForm1.actSaveAsExecute(Sender: TObject);    {Save as}
begin
  if SaveDialog.Execute then
    mmoText.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TForm1.actSaveExecute(Sender: TObject);      {Save settings}
var
  inlist: TStringList;

begin
  inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(OpenDialog.FileName);
    inlist.SaveToFile(ChangeFileExt(OpenDialog.FileName, '.bak'));
    mmoText.Lines.SaveToFile(OpenDialog.FileName);
  finally
    inlist.Free;
  end;
end;

procedure TForm1.cbxPortChange(Sender: TObject);       {select UART port}
begin
  btnBind.Enabled:=true;
end;

end.

