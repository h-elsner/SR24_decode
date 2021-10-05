unit SR24_decode_main;

{Test tool and example to read / write via UART for SR24 Yuneec receiver.

 Uses non standard package "Industrial" - install with Online-Package-Manager.

 Prepare and test UART on Raspi:
 https://buyzero.de/blogs/news/praktische-kommunikation-per-uart-und-rs485-am-raspberry-pi

 Info about SR24:
 https://www.rcgroups.com/forums/showthread.php?2973916-Yuneec-Receiver-protocol
 https://yuneecpilots.com/threads/st16-v1.20747
 https://yuneecpilots.com/threads/typhoon-h-st-16-controller-can-it-be-re-purposed.9970
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin, A3nalogGauge, MKnob, switches, AdvLed, SR24_dec, SR24_ctrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnPWM: TButton;
    btnClosePWM: TButton;
    lblPanMode: TLabel;
    lblRSSIval: TLabel;
    lblPanVal: TLabel;
    lblThrVal: TLabel;
    lblYawVal: TLabel;
    lblRollVal: TLabel;
    lblPitchVal: TLabel;
    ledAux: TAdvLed;
    btnConnect: TButton;
    btnListen: TButton;
    btnListenRaw: TButton;
    btnSave: TButton;
    btnSend: TButton;
    btnSend1: TButton;
    cbGPSonly: TCheckBox;
    cgErrorFlags: TCheckGroup;
    cbGPS: TCheckBox;
    edFmode: TEdit;
    edMotor: TEdit;
    edIMU: TEdit;
    edPCS: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    lblStatus: TLabel;
    ledStop: TAdvLed;
    Memo1: TMemo;
    mPan: TmKnob;
    rgType: TRadioGroup;
    swGear: TOnOffSwitch;
    pLeft: TPanel;
    pRight: TPanel;
    pbRSSI: TProgressBar;
    barLleft: TProgressBar;
    barLdown: TProgressBar;
    barLup: TProgressBar;
    barLright: TProgressBar;
    barRdown: TProgressBar;
    barRright: TProgressBar;
    barRleft: TProgressBar;
    barRup: TProgressBar;
    pbTilt: TProgressBar;
    speGPS: TFloatSpinEdit;
    lblFmode: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    speVx: TFloatSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    speSats: TSpinEdit;
    tbGPIO: TTabSheet;
    tbMirror: TTabSheet;
    TrackBar1: TTrackBar;
    Voltmeter: TA3nalogGauge;
    btnBind: TButton;
    btnStop: TButton;
    btnClose: TButton;
    cbxUARTname: TComboBox;
    CheckGroup1: TCheckGroup;
    edLat: TEdit;
    edLon: TEdit;
    speVolt: TFloatSpinEdit;
    speAmp: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmoProtocol: TMemo;
    pcMain: TPageControl;
    SaveDialog: TSaveDialog;
    speAlt: TFloatSpinEdit;
    tbRaw: TTabSheet;
    tbVolt: TTabSheet;
    Timer1: TTimer;
    procedure btnBindClick(Sender: TObject);
    procedure btnClosePWMClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnListenClick(Sender: TObject);
    procedure btnListenRawClick(Sender: TObject);
    procedure btnPWMClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edFmodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure speVoltChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    function ErrorStatus: byte;
    procedure Sendtest(data: TPayload);

  public
    procedure sr24Con;
    procedure sr24Discon;
    procedure ListenOnSR24;                          {Listen SR24}
    procedure ListenRaw;                             {Listen any byte stream on UART}
    function OutData(data: TPayLoad): boolean;       {Decode and show received packaga}
    procedure SendTelemetry(data: TPayload);         {Send one telemetry dataset}
    procedure WriteProtocol(s: string);              {Catch output}
    procedure UpdateTelemetry(var data: TPayload);
  end;

var
  Form1: TForm1;
  SR24Connected: boolean;

const
  csvheader='Date/Time;MsgType;Counter;?;RSSI[%];PackageCtnr;CH0;CH1;CH2;CH3;CH4;CH5;CH6;CH7;CH8;CH9;CH10;CH11;lat;lon;alt;acc;speed;angle;Num Sats';
  rsNoData='No data to read on UART';
  sep=';';
  cff='0.000000';
  aff='0.0';
  tab1=' ';
  capError='Error flags';

  period=20000000;                                 {Analog servo 20ms}
  asmin=900;
  asmax=2100;
  asmiddle=1500;


implementation

{$R *.lfm}

{ TForm1 }

function ModeLegacy(const f: integer): string;     {Q500, YTH and all other legacy}
begin
  result:='Undefined';
  case f of
     0: result:='Stability';
     1: result:='Stability - GPS off';
     2: result:='Stability - GPSlost';
     3: result:='Angle';
     4: result:='Angle - GPS off';
     5: result:='Angle - GPS lost';
     6: result:='Smart';
     7: result:='Smart - GPS lost';
     8: result:='Motor starting';
     9: result:='Temperature calibration';
    10: result:='Pressure calibration';
    11: result:='Accelerometer bias';
    12: result:='Emergency';
    13: result:='RTH Coming';
    14: result:='RTH Landing';
    15: result:='Binding';
    16: result:='Initializing/Ready';                    {Ready to start}
    17: result:='Waiting on RC';
    18: result:='Magnetomer calibration';
    19: result:='Unknown';
    20: result:='Agility/Rate';                         {Rate}
    21: result:='Smart - Follow me';
    22: result:='Smart - Follow me - GPS lost';
    23: result:='Smart - Camera tracking';
    24: result:='Camera tracking - GPS lost';
    26: result:='Task Curve Cable Cam';
    27: result:='Task Journey';
    28: result:='Task Point of Interest';
    29: result:='Task Orbit';
    32: result:='IPS';                             {FMODE_ANGLE_MODE_IPS_ONLY:I = 0x20}
    33: result:='Waypoints';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:='Record SR24 data stream';
  mmoProtocol.Text:='';
  mmoProtocol.Font.Name:='Liberation Mono';
  Memo1.Lines.Clear;
  btnStop.Tag:=0;
  btnListen.Tag:=1;
  Timer1.Tag:=1;
  Timer1.Enabled:=false;
  SR24connected:=false;
  Voltmeter.Position:=speVolt.Value;
  lblFmode.Caption:=ModeLegacy(StrToIntDef(edFmode.Text, 16));
  cgErrorFlags.Caption:=capError;
  mPan.Position:=m50Val;                           {1365 = 50%}
  TrackBar1.Enabled:=false;
end;

procedure TForm1.speVoltChange(Sender: TObject);
begin
  Voltmeter.Position:=speVolt.Value;
end;

function MessageTypeToStr(mtp: byte):string;
begin
  result:='Unknown '+IntToStr(mtp);
  case mtp of
    0: result:='Data12Ch';
    1: result:='Data24Ch';
    2: result:='Telemetry';
    3: result:='GPS data';
    4: Result:='Bind mode';
  end;
end;


function RawData(data: TPayLoad; len: byte): string;
var
  i: integer;
begin
  result:='';
  for i:=0 to len do
    result:=result+HexStr(data[i], 2)+tab1;
end;

function ChannelValues(data: TPayLoad; numch: byte): string;
var
  i: integer;

begin
  result:='';
  for i:=0 to numch-1 do begin
    result:=result+IntToStr(GetChValue(data, i))+sep;
  end;
end;

procedure TForm1.WriteProtocol(s: string);        {Catch output}
begin
  if pcMain.ActivePage=tbRaw then
    mmoProtocol.Lines.Add(s);
end;

{Data structure from ChannelDataForward.java}
function GPSdata(data: TPayLoad): string;
var
  lat, lon, alt: single;

begin
  result:=ChannelValues(data, 12);                                   {12 channels}
  if GetGPSdata(data, lat, lon, alt) then
    result:=result+
            FormatFloat(cff, lat)+sep+
            FormatFloat(cff, lon)+sep+
            FormatFloat(aff, alt)+sep+
            FormatFloat(aff, GetIntFromBuf(data, 38, 2)/10)+sep+     {acc}
            FormatFloat(aff, GetIntFromBuf(data, 40, 2)/100)+sep+    {speed}
            FormatFloat(aff, GetIntFromBuf(data, 42, 2)/100)+sep+    {angle}
            IntToStr(GetNumSat(data[44]));                           {Num sats}
end;

procedure TForm1.sr24Con;                            {Added color indication}
begin
  ConnectUART(trim(cbxUARTname.Text), SR24connected);
  if SR24connected then
    cbxUartname.Color:=clGradientActiveCaption;
  btnClose.Enabled:=false;
end;

procedure TForm1.sr24Discon;                         {Added color indication}
begin
  DisconnectUART(SR24connected);
  if not SR24connected then
    cbxUartname.Color:=clGradientInactiveCaption;
  btnClose.Enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);       {sends 5 times bind message}
begin
  Timer1.Enabled:=false;
  SendBind;
  Timer1.Tag:=Timer1.Tag+1;
  if Timer1.Tag>5 then begin
    Timer1.Tag:=1;
  end else
    Timer1.Enabled:=true;
end;

function TForm1.OutData(data: TPayLoad): boolean;    {Decode and show received package}
var
  len, mtp: byte;
  s: string;

begin
  len:=data[2];
  mtp:=data[3];                                      {Message type}
  result:=TestCRC8(data, len);                       {CRC check}
  if result then begin
    if (cbGPSonly.Checked and (mtp=3)) or
       (not cbGPSonly.Checked) then begin
      s:=FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', now)+sep; {Create timestamp}
      s:=s+MessageTypeToStr(mtp)+sep;                {Message type}
      s:=s+IntToStr(data[4])+sep;                    {Message counter from SR24, not used?}
      s:=s+HexStr(data[5], 2)+sep;
      s:=s+IntToStr(GetRSSI(data))+sep;              {RSSI in %}
      s:=s+IntToStr(data[7])+sep;                    {Number of UART packets sent since reception of last RF frame (this tells something about age / rate)}
      case mtp of
        0: s:=s+ChannelValues(data, 12);             {12 channels à 12 bit}
        1: s:=s+ChannelValues(data, 24);             {24 channels à 12 bit}
        3: s:=s+GPSdata(data);
      else
        s:=RawData(data, len+3);                     {Other unknown messages as hex stream}
      end;
      WriteProtocol(s);                      {CSV output}
      mmoProtocol.SelStart:=length(mmoProtocol.Text);
    end;
  end else begin
    WriteProtocol('Wrong checksum: '+RawData(data, len+3));
  end;
  Application.ProcessMessages;                       {Just to get the stop button}
end;

procedure TForm1.btnListenClick(Sender: TObject);    {Read datasets}
begin
  btnBind.Enabled:=false;
  btnListenRaw.Enabled:=false;
  ListenOnSR24;
  if btnListen.Tag>100 then
    btnListen.Tag:=1;
end;

procedure TForm1.btnBindClick(Sender: TObject);      {Bind procedure}
begin
  btnStop.Enabled:=true;
  btnListenRaw.Enabled:=false;
  btnListen.Enabled:=false;
  Timer1.Tag:=1;
  Timer1.Enabled:=true;
  sr24Con;
end;

procedure TForm1.btnListenRawClick(Sender: TObject); {Read raw bytes}
begin
  btnBind.Enabled:=false;
  btnListen.Enabled:=false;
  ListenRaw;
  if btnListen.Tag>100 then
    btnListen.Tag:=1;
end;

procedure TForm1.btnPWMClick(Sender: TObject);
var
  status: byte;

begin
  TrackBar1.Position:=asmiddle;    {1500 micro sec}
  btnClose.Enabled:=false;
  status:=PWMstatus;
  Memo1.Lines.Add('PWM status: '+IntToStr(status));
  if status=0 then begin
    Memo1.Lines.Add('Hardware PWM not activated.');
    Memo1.Lines.Add('Add "dtoverlay=pwm-2chan,pin=18,func=2,pin2=13,func2=4" to /boot/config.txt.');
  end else begin
    Memo1.Lines.Add('Activate PWM...');
    ActivatePWMChannels;
    status:=PWMstatus;
    Memo1.Lines.Add('PWM status now:  '+IntToStr(status));
    if status>1 then begin
      Memo1.Lines.Add('Set PWM freqency...');
      sleep(100);
      SetPWMChannel(0, period, TrackBar1.Position*1000);
      SetPWMChannel(1, period, asmiddle*1000);
      TrackBar1.Enabled:=true;
    end;
  end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  setPWMCycle(0, TrackBar1.Position*1000);         {micro seconds}
end;

procedure TForm1.btnClosePWMClick(Sender: TObject);
begin
  TrackBar1.Enabled:=false;
  DeactivatePWM;
  Memo1.Lines.Add('PWM status deactivated: '+inttostr(PWMstatus));
  btnClose.Enabled:=true;
end;

function StrToCoord(coord: string): single;
var
  s: string;

begin
  result:=11.11;
  if length(coord)>0 then begin
    s:=StringReplace(coord, '.', DefaultFormatSettings.DecimalSeparator, []);
    s:=StringReplace(s, ',', DefaultFormatSettings.DecimalSeparator, []);
    result:=StrToFloatDef(s, 10.11);
  end;
end;

function TForm1.ErrorStatus: byte;                   {Creates error flags}
var i, e: byte;

begin
  result:=0;
  e:=1;
  for i:=0 to 7 do begin
    if cgErrorFlags.Checked[i] then
      result:=result or e;
    e:=e shl 1;
  end;
  cgErrorFlags.Caption:=capError+tab1+BinStr(result, 8);
end;

procedure TForm1.UpdateTelemetry(var data: TPayload);
begin
  IntToTelemetry(data, CoordToInt(StrToCoord(edLat.text)), 6, 4);  {lat}
  IntToTelemetry(data, CoordToInt(StrToCoord(edLon.text)), 10, 4); {lon}
  IntToTelemetry(data, AltitudeToInt(speAlt.Value), 14, 4);        {Altitude m}
  IntToTelemetry(data, AltitudeToInt(speVx.Value), 18, 2);         {vx Speed m/s}
  data[24]:=speSats.Value;
  if cbGPS.Checked then
    data[24]:=speSats.Value or $80;                  {nsat, GPS used}
  data[25]:=VoltToByte(speVolt.Value);               {voltage}
  data[26]:=CurrentToByte(speAmp.Value);             {current}

  data[27]:=0;    {roll}
  data[28]:=0;
  data[29]:=0;    {pitch}
  data[30]:=0;
  data[31]:=0;    {yaw}
  data[32]:=0;

  data[33]:=StrToIntDef(edMotor.Text, 255);          {Motorstatus}
  data[34]:=StrToIntDef(edIMU.Text, 97);             {IMU status}
  data[35]:=StrToIntDef(edPCS.Text, 85);             {Press_compass_status}
  data[36]:=StrToIntDef(edFmode.Text, 16);           {f_mode}
  data[37]:=rgType.ItemIndex+1;                      {Set vehicle type}

  data[38]:=Errorstatus;                             {error flags}
  data[39]:=round(speGPS.Value*20) and $FF;          {GPSAccH}

end;

procedure TForm1.btnSendClick(Sender: TObject);      {Button send}
var
  data: TPayLoad;
  i, z: integer;

begin
  btnStop.Enabled:=true;
  btnStop.Tag:=0;
  mmoProtocol.Lines.Clear;
  z:=0;
  for i:=0 to 39 do
    data[i]:=DefTelemetry[i];                        {Set to default}
  sr24Con;
  if UARTCanWrite then begin
    repeat
      UpdateTelemetry(data);
      IntToTelemetry(data, z, 4, 2);
      SendTelemetry(data);
      sleep(200);
      Application.ProcessMessages;
      inc(z);
      if z>=65535 then
        z:=0;
    until (btnStop.Tag>0);
  end else
    WriteProtocol('No connection');
  sr24Discon;
end;

procedure TForm1.SendTest(data: TPayload);
var
  i, crc: byte;
  s: string;

begin
  SaveDialog.FileName:='SentTelemetry_'+IntToStr(btnListen.Tag)+'.csv';
  s:='';
  for i:=0 to data[2]+1 do begin
    UARTwriteByte(data[i]);
    s:=s+HexStr(data[i], 2)+sep;
  end;
  crc:=SR24_CRC8(data, data[2]);
  UARTwriteByte(crc);
  mmoProtocol.Lines.Add(s+HexStr(crc, 2));
end;

procedure TForm1.SendTelemetry(data: TPayload);      {Send one telemetry dataset}
var
  crc, i: byte;
  s: string;

begin
  SaveDialog.FileName:='SentTelemetry_'+IntToStr(btnListen.Tag)+'.csv';
  s:='Send telemetry'+sep;
  for i:=0 to data[2]+1 do begin
    UARTwriteByte(data[i]);
    s:=s+HexStr(data[i], 2)+sep;
  end;
  crc:=SR24_CRC8(data, data[2]);
  UARTwriteByte(crc);
  WriteProtocol(s+HexStr(crc, 2));
end;

procedure TForm1.btnConnectClick(Sender: TObject);   {Mirroring}
var
  data, tele: TPayLoad;
  i, z: byte;
  tlz, gps: uint16;
  coord: array [0..7] of byte;
  alt: single;
  thr, roll, pitch, yaw, pan: uint16;

begin
  btnStop.Tag:=0;
  btnStop.Enabled:=true;
  z:=0;
  tlz:=0;
  for i:=0 to 39 do                                  {Load default values}
    tele[i]:=DefTelemetry[i];
  tele[37]:=rgType.ItemIndex+1;                      {Set vehicle type}
  for i:=0 to high(data) do                          {Empty receive buffer}
    data[i]:=0;
  for i:=0 to high(coord) do                         {Empty receive buffer}
    coord[i]:=0;
  alt:=0;
  sr24Con;
  if UARTCanRead then begin
    lblStatus.Caption:='Connected';
    repeat
      if UARTreadMsg(data) then begin
        if data[3]=3 then begin                      {GPS data set}
          for i:=0 to 7 do
            coord[i]:=data[26+i];                    {Store coordinates}
          alt:=GetFloatFromBuf(data, 34);            {Store altitude}
        end;
        GetSticks(data, thr, roll, pitch, yaw);      {Sticks}
        barLup.Position:=thr-stkntrl;
        barLdown.Position:=stkntrl-thr;
        barRLeft.Position:=roll-stkntrl;
        barRright.Position:=stkntrl-roll;
        barLleft.Position:=yaw-stkntrl;
        barLright.Position:=stkntrl-yaw;
        barRup.Position:=pitch-stkntrl;
        barRdown.Position:=stkntrl-pitch;
        lblThrVal.Caption:=IntToStr(thr)+'='+IntToStr(StkToProz(thr))+'%';
        lblPitchVal.Caption:=IntToStr(pitch)+'='+IntToStr(StkToProz(pitch))+'%';
        lblRollVal.Caption:=IntToStr(roll)+'='+IntToStr(StkToProz(roll))+'%';
        lblYawVal.Caption:=IntToStr(yaw)+'='+IntToStr(StkToProz(yaw))+'%';

        if thr=0 then begin                          {Mixed button -  start/stop}
          ledStop.State:=lsOn;
          barLdown.Position:=0;

        end else
          ledStop.State:=lsDisabled;

        pbTilt.Position:=GetChValue(data, 6);        {Slider}
        pan:=GetChValue(data, 7);
        mPan.Position:=stkup-pan;                    {Knob}
        lblPanVal.Caption:=IntToStr(pan)+'='+IntToStr(StkToProz(pan))+'%';

        case GetChValue(data, 9) of                  {Example switch - Panmode}
          stkdown: lblPanMode.Caption:='Follow mode';
          m45val:  lblPanMode.Caption:='Team mode';
          m40val:  lblPanMode.Caption:='Follow pan controllable';
          stkup:   lblPanMode.Caption:='Global mode';
        end;
        case GetChValue(data, 10) of                 {Gear Switch}
          stkmin: swGear.Checked:=false;             {0 - up}
          stkmax: swGear.Checked:=true;              {4095 - down}
        end;
        case GetChValue(data, 11) of                 {Example Push button}
          stkmin: ledAux.State:=lsOn;                {Aux on}
          stkmax: ledAux.State:=lsDisabled;          {Aux off}
        end;
        inc(z);

        if z>=4 then begin                           {One telemetry per 5 received packages}
          gps:=0;
          IntToTelemetry(data, tlz, 4, 2);           {Counter}
          for i:=0 to 7 do begin
            tele[i+6]:=coord[i];                     {Mirror coordinates}
            gps:=gps+Coord[i];                       {Check controller GPS}
          end;
          IntToTelemetry(tele, AltitudeToInt(alt), 14, 4);  {Mirror Altitude m}
          pbRSSI.Position:=GetRSSI(data);            {Show RSSI level}
          lblRSSIval.Caption:=IntToStr(data[6])+'='+IntToStr(GetRSSI(data))+'%';
          case GetChValue(data, 4) of
            stkdown: tele[36]:=13;                   {683 - RTH coming}
            stkntrl: tele[36]:=3;                    {2048 - Angle mode}
            stkup:   tele[36]:=6;                    {3412 - Smart}
          end;
          if thr=0 then
            tele[36]:=16;                            {Back to Ready if stop button is pressed}
          i:=data[44];                               {nsat}
          if gps>0 then
            i:=i or $80;                             {GPS aquired}
          tele[24]:=i;                               {nsat + GPS used}
          UARTsendMsg(tele);
          z:=0;
          inc(tlz);                                  {Counter for sent packages}
          if tlz>=65535 then
            tlz:=0;
        end;
        Application.ProcessMessages;
      end;
    until btnStop.Tag>0;                              {Listen until Stop}
  end else
    lblStatus.Caption:=rsNoData;
  sr24Discon;
end;

procedure TForm1.ListenRaw;
var
  b: byte;
  zeile: string;

begin
  SaveDialog.FileName:='UART_YTH_'+IntToStr(btnListen.Tag)+'.txt';
  btnStop.Tag:=0;
  btnStop.Enabled:=true;
  mmoProtocol.Lines.Clear;
  sr24Con;
  if UARTCanRead then begin
    zeile:='';
    repeat                                           {Listen on UART}
      b:=UARTreadbyte;                               {read byte by byte}
      if (b=header1) and (length(zeile)>15) then begin
        WriteProtocol(zeile);
        zeile:='';
      end;
      zeile:=zeile+tab1+HexStr(b, 2);                {Hex values}
      Application.ProcessMessages;
    until btnStop.Tag>0;                             {Listen until Stop}
  end else
    WriteProtocol(rsNoData);
  sr24Discon;
end;

procedure TForm1.ListenOnSR24;                       {Listen SR24}
var
  data: TPayLoad;

begin
  SaveDialog.FileName:='UARTsr24_'+IntToStr(btnListen.Tag)+'.csv';
  btnStop.Tag:=0;
  btnStop.Enabled:=true;
  mmoProtocol.Lines.Clear;
  sr24Con;
  if UARTCanRead then begin
    WriteProtocol(csvheader);
    repeat                                           {Listen on UART}
      if UARTreadMsg(data) then
        OutData(data);
    until btnStop.Tag>0;                             {Listen until Stop}
  end else
    WriteProtocol(rsNoData);
  sr24Discon;
end;

procedure TForm1.btnStopClick(Sender: TObject);      {Stop}
begin
  Timer1.Enabled:=false;
  btnStop.Tag:=1;
  btnStop.Enabled:=false;
  btnBind.Enabled:=true;
  btnConnect.Enabled:=true;
  btnListenRaw.Enabled:=true;
  btnListen.Enabled:=true;
end;

procedure TForm1.btnSaveClick(Sender: TObject);      {Save}
begin
  if SaveDialog.Execute then begin
    mmoProtocol.Lines.SaveToFile(SaveDialog.FileName);
    mmoProtocol.Lines.Clear;
    btnListen.Tag:=btnListen.Tag+1;
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);     {Close}
begin
  Timer1.Enabled:=false;
  btnStop.Tag:=1;
  sleep(200);
  Close;
end;

procedure TForm1.edFmodeChange(Sender: TObject);
begin
  lblFmode.Caption:=ModeLegacy(StrToIntDef(edFmode.Text, 16));
end;

end.

