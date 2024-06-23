          {********************************************************}
          {                                                        }
          {   Control RC model with ST10/ST16 with SR24 receiver   }
          {                                                        }
          {       Copyright (c) 2021-2022    Helmut Elsner         }
          {                                                        }
          {       Compiler: FPC 3.2.2   /    Lazarus 2.2.0         }
          {                                                        }
          { Pascal programmers tend to plan ahead, they think      }
          { before they type. We type a lot because of Pascal      }
          { verboseness, but usually our code is right from the    }
          { start. We end up typing less because we fix less bugs. }
          {           [Jorge Aldo G. de F. Junior]                 }
          {********************************************************}

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
  ComCtrls, Spin, MKnob, switches, AdvLed,
  SR24_dec, SR24_ctrl, SR24_chsets, SR24_log;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnBind: TButton;
    btnLog: TButton;
    cbxUARTspeed: TComboBox;
    cgIMU: TCheckGroup;
    cgPCS: TCheckGroup;
    Edit1: TEdit;
    Label17: TLabel;
    lblAmp: TLabel;
    lblStatus1: TLabel;
    lblYaw: TLabel;
    lblRoll: TLabel;
    lblPitch: TLabel;
    lblFixType: TLabel;
    lblThrVal: TLabel;
    lblPanMode: TLabel;
    lblRSSIval: TLabel;
    lblPanVal: TLabel;
    lblVy: TLabel;
    lblVz: TLabel;
    lblYawVal: TLabel;
    lblRollVal: TLabel;
    lblPitchVal: TLabel;
    ledAux: TAdvLed;
    btnConnect: TButton;
    btnListen: TButton;
    btnListenRaw: TButton;
    btnSave: TButton;
    btnSend: TButton;
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
    mPan: TmKnob;
    rgType: TRadioGroup;
    spePitch: TFloatSpinEdit;
    speRoll: TFloatSpinEdit;
    speYaw: TFloatSpinEdit;
    speVy: TFloatSpinEdit;
    speVz: TFloatSpinEdit;
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
    lblMot: TLabel;
    lblPCS: TLabel;
    lblIMU: TLabel;
    speVx: TFloatSpinEdit;
    lblVolt: TLabel;
    lblVx: TLabel;
    speSats: TSpinEdit;
    tbMirror: TTabSheet;
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
    pcTests: TPageControl;
    SaveDialog: TSaveDialog;
    speAlt: TFloatSpinEdit;
    tbRaw: TTabSheet;
    tbVolt: TTabSheet;
    tmBind: TTimer;
    procedure btnBindClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnListenClick(Sender: TObject);
    procedure btnListenRawClick(Sender: TObject);
    procedure btnLogClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cgIMUClick(Sender: TObject);
    procedure cgPCSClick(Sender: TObject);
    procedure edFmodeChange(Sender: TObject);
    procedure edIMUChange(Sender: TObject);
    procedure edMotorChange(Sender: TObject);
    procedure edPCSChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure speSatsChange(Sender: TObject);
    procedure tmBindTimer(Sender: TObject);
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
  csets: TSettings;

const
  csvheader='Date/Time;MsgType;Counter;?;RSSI[%];PackageCtnr;CH0;CH1;CH2;CH3;CH4;CH5;CH6;CH7;CH8;CH9;CH10;CH11;lat;lon;alt;acc;speed;angle;Num Sats';
  rsNoData='No data to read on UART';
  rsFixType='Fix type';
  rsFmode='Flight mode';
  rsIMU='IMU status';
  rsPCS='Press/Comp status';
  rsMot='Motor status';
  rsConnected='Connected';
  rsDisconnected='Disconnected';
  tab1=' ';
  dpkt=': ';
  capError='Error flags';
  VehicleID='20';                            {q500log2kml: 20..29 car, 30..39 boat}

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
    16: result:='Initializing/Ready';              {Ready to start}
    17: result:='Waiting on RC';
    18: result:='Magnetomer calibration';
    19: result:='Unknown';
    20: result:='Agility/Rate';                    {Rate}
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
  btnStop.Tag:=0;
  btnListen.Tag:=1;
  tmBind.Tag:=1;
  tmBind.Enabled:=false;
  SR24connected:=false;
  lblFmode.Caption:=rsFmode;
  cgErrorFlags.Caption:=capError;
  mPan.Position:=m50Val;                           {1365 = 50%}
  ReadSettings(csets);                             {Load common settings}
  lblStatus.Caption:=rsDisconnected;
  lblStatus1.Caption:=rsDisconnected;
end;

procedure TForm1.speSatsChange(Sender: TObject);
var ft: byte;

begin
  ft:=speSats.Value and $FF;
  speSats.Hint:=IntToStr(ft and $1F)+' sats';
  if ft and $80>0 then
    cbGPS.Checked:=true;                           {nsat, GPS used}
  lblFixType.Caption:=rsFixtype+dpkt+LineEnding+FixTypeToStr(ft);
end;

function MessageTypeToStr(mtp: byte):string;       {Known message types as string}
begin
  result:='Unknown '+IntToStr(mtp)+tab1+'($'+HexStr(mtp, 2)+')';
  case mtp of
    0:  result:='Data12Ch';
    1:  result:='Data24Ch';
    2:  result:='Telemetry';
    3:  result:='GPS data';
    4:  result:='Bind mode';
    20: result:='Commands';
  end;
end;


function RawData(data: TPayLoad; len: byte): string;
var
  i: byte;
begin
  result:='';
  for i:=0 to len do
    result:=result+HexStr(data[i], 2)+tab1;
end;

function ChannelValues(data: TPayLoad; numch: byte): string;
var
  i: byte;

begin
  result:='';
  for i:=1 to numch do begin
    result:=result+IntToStr(GetChValue(data, i))+sep;
  end;
end;

{Data structure from ChannelDataForward.java}
function GPSdata(data: TPayLoad): string;
var
  lat, lon, alt: single;

begin
  result:=ChannelValues(data, 12);                                   {12 channels}
  if GetGPSdata(data, lat, lon, alt) then
    result:=result+
            FormatFloat(cff, lon)+sep+
            FormatFloat(cff, lat)+sep+
            FormatFloat(aff, alt)+sep+
            FormatFloat(aff, GetIntFromBuf(data, 38, 2)/10)+sep+     {acc}
            FormatFloat(aff, GetIntFromBuf(data, 40, 2)/100)+sep+    {speed}
            FormatFloat(aff, GetIntFromBuf(data, 42, 2)/100)+sep+    {angle}
            IntToStr(GetNumSat(data[44]));                           {Num sats}
end;

function Payld(data: TPayLoad): string;
var
  i: byte;

begin
  result:=FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', now)+sep+{Create timestamp}
          MessageTypeToStr(data[3]);                 {Message type}
  for i:=4 to data[2]+2 do
    result:=result+sep+HexStr(data[i], 2);
end;

procedure TForm1.WriteProtocol(s: string);           {Catch output}
begin
  if pcTests.ActivePage=tbRaw then
    mmoProtocol.Lines.Add(s);
end;

procedure TForm1.sr24Con;                            {Added color indication}
var
  bps: uint32;

begin
  case cbxUARTspeed.ItemIndex of
    0: bps:=UARTspeed;                              {SR24 default speed}
    1: bps:=230400;
    2: bps:=500000;
    3: bps:=921600;
  end;
  lblStatus1.Caption:=ConnectUART(trim(cbxUARTname.Text), bps, SR24connected);
  if SR24connected then begin
    cbxUartname.Color:=clGradientActiveCaption;
//    lblConnected.Caption:=rsConnected;
  end;
  btnClose.Enabled:=false;
end;

procedure TForm1.sr24Discon;                         {Added color indication}
begin
  DisconnectUART(SR24connected);
  if not SR24connected then begin
    cbxUartname.Color:=clGradientInactiveCaption;
//    lblConnected.Caption:=rsDisconnected;
  end;
  btnClose.Enabled:=true;
end;

procedure TForm1.tmBindTimer(Sender: TObject);       {sends 5 times bind message}
begin
  tmBind.Enabled:=false;
  SendBind;
  tmBind.Tag:=tmBind.Tag+1;
  if tmBind.Tag>5 then begin
    tmBind.Tag:=1;
  end else
    tmBind.Enabled:=true;
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
      s:=FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', now)+sep+ {Create timestamp}
         MessageTypeToStr(mtp)+sep+                  {Message type}
         IntToStr(data[4])+sep+                      {Message counter from SR24, not used?}
         HexStr(data[5], 2)+sep+
         IntToStr(GetRSSI(data))+sep+                {RSSI in %}
         IntToStr(data[7])+sep;                      {Number of UART packets sent since reception of last RF frame (this tells something about age / rate)}
      case mtp of
        0: s:=s+ChannelValues(data, 12);             {12 channels à 12 bit}
        1: s:=s+ChannelValues(data, 24);             {24 channels à 12 bit}
        3: s:=s+GPSdata(data);
        20: s:=Payld(data);
      else
        s:=RawData(data, len+2);                     {Other unknown messages as hex stream}
      end;
      WriteProtocol(s);                              {CSV output}
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
  tmBind.Tag:=1;
  sr24Con;
  if UARTCanWrite then
    tmBind.Enabled:=true
  else
    lblStatus1.Caption:='No connection';
end;

procedure TForm1.btnListenRawClick(Sender: TObject); {Read raw bytes}
begin
  btnBind.Enabled:=false;
  btnListen.Enabled:=false;
  ListenRaw;
  if btnListen.Tag>100 then
    btnListen.Tag:=1;
end;

procedure TForm1.btnLogClick(Sender: TObject);       {Just to test log functions}
var
  f: TextFile;
  fnr: string;
  i: integer;

begin
  if makeflightlogdir then begin                     {Create FlightLog dirs}
    fnr:=getlognumber;                               {Find next log file}
    AssignFile(f, GetFlightLogDirs(1)+telemetry+fnr);
    try
      append(f);
      for i:=1 to 10 do                              {Append to the log files}
        writeln(f, gettimestamp+sep+IntToStr(i)+sep);
    finally
      CloseFile(f);
    end;
  end;
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

procedure SetBit(cg: TCheckGroup; bl: byte);         {Read Checkgroup items}
var
  i, e: byte;

begin
  e:=1;
  for i:=0 to 7 do begin
    cg.Checked[i]:=((bl and e)>0);
    e:=e shl 1;
  end;
end;

function BitSet(cg: TCheckGroup): byte;              {Set CheckGroup items}
var
  i, e: byte;

begin
  result:=0;
  e:=1;
  for i:=0 to 7 do begin
      if cg.Checked[i] then
        result:=result or e;
      e:=e shl 1;
  end;
end;

function TForm1.ErrorStatus: byte;                   {Creates error flags}
begin
  result:=BitSet(cgErrorFlags);
  cgErrorFlags.Caption:=capError+tab1+BinStr(result, 8);
end;

procedure TForm1.UpdateTelemetry(var data: TPayload);
begin
  IntToTelemetry(data, CoordToInt(StrToCoord(edLat.text)), 6, 4);  {lat}
  IntToTelemetry(data, CoordToInt(StrToCoord(edLon.text)), 10, 4); {lon}
  IntToTelemetry(data, AltitudeToInt(speAlt.Value), 14, 4);        {Altitude m}
  IntToTelemetry(data, AltitudeToInt(speVx.Value), 18, 2);         {vx Speed m/s}
  IntToTelemetry(data, AltitudeToInt(speVy.Value), 20, 2);         {vy Speed m/s}
  IntToTelemetry(data, AltitudeToInt(speVz.Value), 22, 2);         {vz Speed m/s}
  IntToTelemetry(data, AltitudeToInt(spePitch.Value), 27, 2);      {Pitch}
  IntToTelemetry(data, AltitudeToInt(speRoll.Value), 29, 2);       {Roll}
  IntToTelemetry(data, AltitudeToInt(speYaw.Value), 31, 2);        {Yaw}
  data[24]:=speSats.Value;
  if cbGPS.Checked then
    data[24]:=speSats.Value or $80;                  {nsat, GPS used}
  data[25]:=VoltToByte(speVolt.Value);               {voltage}
  data[26]:=CurrentToByte(speAmp.Value);             {current}

  data[33]:=StrToIntDef(edMotor.Text, 255);          {Motorstatus}
  data[34]:=StrToIntDef(edIMU.Text, 97);             {IMU status}
  data[35]:=StrToIntDef(edPCS.Text, 85);             {Press_compass_status}
  data[36]:=StrToIntDef(edFmode.Text, 16);           {f_mode}
  data[37]:=rgType.ItemIndex+1;                      {Set vehicle type}

  data[38]:=Errorstatus;                             {error flags}
//  data[39]:=round(speGPS.Value*20) and $FF;          {GPSAccH like in flight logs}
  data[39]:=round(speGPS.Value) and $FF;             {GPSAccH}

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

procedure InitServos;                                {Initialize Servos. Only two can get HW PWM channels 0 or 1}
var
  i: byte;

begin
  for i:= 1 to 6 do begin                            {For all 6 servos}
    if csets[i, 4]<2 then begin
      SetPWMChannel(csets[i, 4], csets[12, 3],
                    csets[i, 2]*1000,                {Neutral position}
                    (csets[i, 5]=1));
    end;
  end;
end;

procedure ControlServos(dat: TPayLoad);              {Write pulse duration to PWM}
var
  i, pio: byte;

begin
  for i:=1 to 6 do begin                             {For all 6 servos}
    pio:=csets[i, 4];
    if pio<2 then
      SetPWMCycle(pio, StkToPWM(csets, i, GetChValue(dat, csets[i, 0])));
  end;
end;

function GetPWMval(dat: TPayLoad; pwmch: byte=0): integer;   {Get PWM value for speed}
var
  i, pio: byte;

begin
  result:=stkntrl;
  for i:=1 to 6 do begin                             {For all 6 servos}
    pio:=csets[i, 4];
    if (pio<2) and (pio=pwmch) then
      result:=GetChValue(dat, csets[i, 0]);
  end;
end;

procedure GPIOon;                                    {Switch all used GPIO ports to out/0}
var
  i: byte;

begin
  for i:=1 to 11 do begin                            {Activate GPIO pins}
    if ValidGPIOnr(csets[i, 4]) then
      ActivateGPIO(csets[i, 4], 0);
    if ValidGPIOnr(csets[i, 5]) then                 {Will also activate GPIO pins gpio2}
      ActivateGPIO(csets[i, 5], 0);
  end;

  if ValidGPIOnr(csets[0, 1]) then                   {Voltage warnings}
    ActivateGPIO(csets[0, 1], 1);
  if ValidGPIOnr(csets[0, 2]) then
    ActivateGPIO(csets[0, 2], 1);
end;

procedure GPIOoff;                                   {Switch off all used GPIO ports}
var
  i: byte;

begin
  for i:=1 to 11 do begin
    if ValidGPIOnr(csets[i, 4]) then
      DeActivateGPIO(csets[i, 4]);
    if ValidGPIOnr(csets[i, 5]) then
      DeActivateGPIO(csets[i, 5]);
  end;

  if ValidGPIOnr(csets[0, 1]) then                   {Deactivate ports Voltage warnings}
    DeActivateGPIO(csets[0, 1]);
  if ValidGPIOnr(csets[0, 2]) then
    DeActivateGPIO(csets[0, 2]);
end;

procedure ControlSwitches(dat: TPayLoad);            {Send switches to GPIO port}
var
  i, sw: byte;

begin
  for i:=1 to 11 do begin
    sw:=SwitchPos(csets, i, GetChValue(dat, csets[i, 0]));
    if ValidGPIOnr(csets[i, 4]) then begin           {First GPIO port for upper position as ON}
      if sw=1 then
        SetGPIO(csets[i, 4], GPIOhigh)
      else
        SetGPIO(csets[i, 4], GPIOlow);
    end;

    if ValidGPIOnr(csets[i, 5]) then begin           {For 3-way switches use second GPIO, port}
      if sw=3 then                                   {Switch in lower position as ON}
        SetGPIO(csets[i, 5], GPIOhigh)
      else
        SetGPIO(csets[i, 5], GPIOlow);
    end;
  end;
end;

procedure TForm1.btnConnectClick(Sender: TObject);   {Mirroring}
var
  data, tele: TPayLoad;
  i, z: byte;
  tlz, gps: uint16;
  coord: array [0..7] of byte;
  lat, lon, alt, tas: single;
  thr, roll, pitch, yaw, pan: uint16;
  logfile1: TextFile;
  loglist1: TStringList;
  logfile2: TextFile;
  loglist2: TStringList;
  logfile3: TextFile;
  loglist3: TStringList;
  fnr, csvstr: string;

  procedure WriteLogBlock;                           {Write a whole block from buffer to file}
  var
    i: integer;

  begin
    for i:=0 to loglist1.Count-1 do
      writeln(logfile1, loglist1[i]);
    loglist1.Clear;                                  {Empty log buffer}
    for i:=0 to loglist2.Count-1 do
      writeln(logfile2, loglist2[i]);
    loglist1.Clear;                                  {Empty log buffer}
    for i:=0 to loglist3.Count-1 do
      writeln(logfile3, loglist3[i]);
    loglist1.Clear;                                  {Empty log buffer}
  end;

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
  ReadSettings(csets);                               {Load again common settings}

  btnClose.Enabled:=false;
  if ActivatePWMChannel(true)>1 then begin
    btnClose.Enabled:=false;
    InitServos;                                      {Set up PWM channels}
  end;

  sr24Con;
  if UARTCanRead then begin
    GPIOon;
    lblStatus.Caption:=rsConnected;
    if csets[12, 5]>0 then begin                     {Logging enabled, preparations}
      MakeFlightlogDir;
      fnr:=getlognumber;                             {Find next log file}
      loglist1:=TStringList.Create;                  {Buffer for csv strings telemetry}
      loglist2:=TStringList.Create;                  {Buffer for csv strings Remote}
      loglist3:=TStringList.Create;                  {Buffer for csv strings RemoteGPS}
      AssignFile(logfile1, GetFlightLogDirs(1)+telemetry+fnr);
      AssignFile(logfile2, GetFlightLogDirs(1)+remote+fnr);
      AssignFile(logfile3, GetFlightLogDirs(1)+remotegps+fnr);
      append(logfile1);
      append(logfile2);
      append(logfile3);
    end;
    try
      repeat
        if UARTreadMsg(data) then begin
          alt:=0;
          lat:=0;
          lon:=0;
          if data[3]=3 then begin                    {GPS data set}
            for i:=0 to 7 do
              coord[i]:=data[26+i];                  {Store coordinates}
            GetGPSdata(data, lat, lon, alt);
          end;
          thr:=  GetChValue(data, 1);
          roll:= GetChValue(data, 2);
          pitch:=GetChValue(data, 3);
          yaw:=  GetChValue(data, 4);

          barLup.Position:=thr-stkntrl;
          barLdown.Position:=stkntrl-thr;
          barRLeft.Position:=roll-stkntrl;
          barRright.Position:=stkntrl-roll;
          barLleft.Position:=yaw-stkntrl;
          barLright.Position:=stkntrl-yaw;
          barRup.Position:=pitch-stkntrl;
          barRdown.Position:=stkntrl-pitch;

          lblThrVal.Caption:=IntToStr(StkToPWM(csets, 1, thr) div 1000)+
                             '='+IntToStr(StkToProz(thr))+'%';

          lblPitchVal.Caption:=IntToStr(StkToPWM(csets, 3, pitch) div 1000)+
                             '='+IntToStr(StkToProz(pitch))+'%';

          lblRollVal.Caption:=IntToStr(StkToPWM(csets, 2, roll) div 1000)+
                              '='+IntToStr(StkToProz(roll))+'%';

          lblYawVal.Caption:=IntToStr(StkToPWM(csets, 4, yaw) div 1000)+
                             '='+IntToStr(StkToProz(yaw))+'%';

          if thr=0 then begin                        {Mixed button -  start/stop}
            ledStop.State:=lsOn;
            barLdown.Position:=0;

          end else
            ledStop.State:=lsDisabled;

          pbTilt.Position:=GetChValue(data, 7);      {Slider}
          pan:=GetChValue(data, 8);
          mPan.Position:=stkup-pan;                  {Knob}
          lblPanVal.Caption:=IntToStr(StkToPWM(csets, 6, pan) div 1000)+
                             '='+IntToStr(StkToProz(pan))+'%';
          ControlServos(data);                       {Servo assignement from settings}
          ControlSwitches(data);
          case GetChValue(data, 10) of               {Example switch - Panmode}
            stkdown: lblPanMode.Caption:='Follow mode';
            m45val:  lblPanMode.Caption:='Team mode';
            m40val:  lblPanMode.Caption:='Follow pan controllable';
            stkup:   lblPanMode.Caption:='Global mode';
          end;
          case GetChValue(data, 11) of               {Gear Switch}
            stkmin: swGear.Checked:=false;           {0 - up}
            stkmax: swGear.Checked:=true;            {4095 - down}
          end;
          case GetChValue(data, 12) of               {Example Push button}
            stkmin: ledAux.State:=lsOn;              {Aux on}
            stkmax: ledAux.State:=lsDisabled;        {Aux off}
          end;
          inc(z);

          if csets[12, 5]>0 then begin               {Logging enabled, read RC stuff}
            csvstr:=GetTimeStamp;
            for i:=1 to 12 do
              csvstr:=csvstr+sep+IntToStr(GetChValue(data, i));
            loglist2.Add(csvstr);                    {Remote}
            if data[3]=3 then                        {GPS data -> write RemoteGPS}
              loglist3.Add(GetTimeStamp+sep+GPSdata(data));
          end;

          if z>=4 then begin                         {One telemetry per 5 received packages}
            gps:=0;
            IntToTelemetry(data, tlz, 4, 2);         {Counter}
            for i:=0 to 7 do begin
              tele[i+6]:=coord[i];                   {Mirror coordinates}
              gps:=gps+Coord[i];                     {Check controller GPS}
            end;

            i:=csets[0, 1];
            if (i<notused) and (i>4) and (GetGPIO(i)=GPIOhigh) then
              tele[38]:=(tele[38] or 1) and $FD;     {Voltage warning 1}
            i:=csets[0, 2];
            if (i<notused) and (i>4) and (GetGPIO(i)=GPIOhigh) then
              tele[38]:=(tele[38] or 2) and $FE;     {Voltage warning 2}

            IntToTelemetry(tele, AltitudeToInt(alt), 14, 4);  {Mirror Altitude m}
            pbRSSI.Position:=GetRSSI(data);          {Show RSSI level}
            lblRSSIval.Caption:=IntToStr(data[6])+'='+IntToStr(GetRSSI(data))+'%';
            case GetChValue(data, 5) of
              stkdown: tele[36]:=13;                 {683 - RTH coming}
              stkntrl: tele[36]:=3;                  {2048 - Angle mode}
              stkup:   tele[36]:=6;                  {3412 - Smart}
            end;
            if thr=0 then
              tele[36]:=16;                          {Back to Ready if stop button is pressed}
            i:=data[44];                             {nsat}
            if gps>0 then
              i:=i or $80;                           {GPS aquired}
            tele[24]:=i;                             {nsat + GPS used}
            UARTsendMsg(tele);
            z:=0;
            inc(tlz);                                {Counter for sent packages}
            if tlz>=65535 then
              tlz:=0;
            if csets[12, 5]>0 then begin             {Logging enabled, read telemetry}
              tas:=(GetPWMval(data, 0)-stkntrl)/csets[0, 3];     {Value PWM0 - neutral div corrction value for speed}
              csvstr:=GetTimeStamp+sep+
                      IntToStr(pbRSSI.Position)+sep+'0'+sep+'0'+sep+        {RSSI, V, A}
                      FormatFloat(aff, alt)+sep+                            {Altitude}
                      FormatFloat(cff, lat)+sep+FormatFloat(cff, lon)+sep+  {lat, lon}
                      FormatFloat(aff, tas)+sep;                            {tas from PWM0}
              if gps>0 then
                csvstr:=csvstr+'true'+sep
              else
                csvstr:=csvstr+'false'+sep;                                 {gps_used}
              csvstr:=csvstr+IntToStr(GetFixType(data[44]))+sep+
                             IntToStr(GetNumSat(data[44]))+sep+
              '0,0,0'+sep;                                                  {roll, pitch, yaw}
              for i:=0 to 3 do                                              {Status bytes}
                csvstr:=csvstr+IntToStr(tele[i+33])+sep;
              csvstr:=csvstr+'0'+sep+VehicleID+sep+IntToStr(tele[38])+sep+  {GPS_status, v_type, error_flags}
                      FormatFloat('0.0', GetIntFromBuf(data, 38, 2)/20);    {gps_accH from accuracy}

              loglist1.Add(csvstr);
            end;
            if loglist1.Count>500 then               {Buffer to file}
              WriteLogBlock;
          end;
          Application.ProcessMessages;
        end;
      until btnStop.Tag>0;                           {Listen until Stop}
      if loglist1.Count>0 then                       {Rest of buffer to file}
        WriteLogBlock;
    finally
      if csets[12, 5]>0 then begin                   {Logging enabled, cleanup}
        loglist1.Free;
        CloseFile(logfile1);
        loglist2.Free;
        CloseFile(logfile2);
        loglist3.Free;
        CloseFile(logfile3);
      end;
    end;
  end else
    lblStatus.Caption:=rsNoData;
  sr24Discon;
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
  SetBit(cgIMU, StrToIntDef(edIMU.Text, 97));
  SetBit(cgPCS, StrToIntDef(edPCS.Text, 85));
  sr24Con;
  if UARTCanWrite then begin
    lblStatus1.Caption:=rsConnected;
    repeat
      UpdateTelemetry(data);                         {Counter}
      IntToTelemetry(data, z, 4, 2);
      SendTelemetry(data);
      Application.ProcessMessages;
      inc(z);
      if z>=65535 then
        z:=0;
    until (btnStop.Tag>0);
  end else
    WriteProtocol('No connection');
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
  tmBind.Enabled:=false;
  btnStop.Tag:=1;
  btnStop.Enabled:=false;
  btnBind.Enabled:=true;
  btnClose.Enabled:=true;
  btnConnect.Enabled:=true;
  btnListenRaw.Enabled:=true;
  btnListen.Enabled:=true;
  DeactivatePWM;
  btnClose.Enabled:=true;
  GPIOoff;
  sr24Discon;
  lblStatus.Caption:=rsDisconnected;
  lblStatus1.Caption:=rsDisconnected;
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
  tmBind.Enabled:=false;
  btnStop.Tag:=1;
  Close;
end;

procedure TForm1.cgIMUClick(Sender: TObject);
begin
  edIMU.Text:=IntToStr(BitSet(cgIMU));
end;

procedure TForm1.cgPCSClick(Sender: TObject);
begin
  edPCS.Text:=IntToStr(BitSet(cgPCS));
end;

procedure TForm1.edFmodeChange(Sender: TObject);
begin
  lblFmode.Caption:=rsFmode+dpkt+LineEnding+
                    ModeLegacy(StrToIntDef(edFmode.Text, 16));
end;

procedure TForm1.edIMUChange(Sender: TObject);
begin
  lblIMU.Caption:=rsIMU+dpkt+LineEnding+
                  BinStr(StrToIntDef(edIMU.Text, 0), 8);
end;

procedure TForm1.edMotorChange(Sender: TObject);
begin
  lblMot.Caption:=rsMot+dpkt+LineEnding+
                  BinStr(StrToIntDef(edMotor.Text, 0), 8);
end;

procedure TForm1.edPCSChange(Sender: TObject);
begin
  lblPCS.Caption:=rsPCS+dpkt+LineEnding+
                  BinStr(StrToIntDef(edPCS.Text, 0), 8);
end;

end.

