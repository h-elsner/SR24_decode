{From ST24.h and ST24.c PX4 Autopilot

 Preparations:
 -------------
 Add to /boot/config.txt:
 [Switch-off bluetooth to get serial]
  dtoverlay=pi3-disable-bt

 Switch-off UART console:
 sudo raspi-config > Interface Options > Serial port >
 "Would you like a login shell to be accessible over serial?" --> No
 "Would you like the serial port hardware to be enabled?" --> Yes


 Read data from SR24
 -------------------

 Uses non-standard package "Synapse" (install per Online package manager).
 Then open package file laz_synapse.lpk and Add to project.

 Data format UART messages (data packages)

byte idx val   desrcription
 0       $55   header 1
 1       $55   header 2
 2       len   24/43 length data after len byte inclusive type and CRC8, max 64
 3       0..3  Msg type: CHANNELDATA12      = 0                len $18  24
	                 CHANNELDATA24      = 1
                         Telemetry to RC    = 2                len $26  38
	                 TRANSMITTERGPSDATA = 3                len $2B  43
                         BIND               = 4
                         Commands           = 20
 4       Counter         0 for old SR24 FW (Q500)
 5       ??    Random?   0 for old SR24 FW (Q500)
 6       RSSI  (in % ?)
 7       Package counter  (lost packages?)
 8       from here on Payload bytes (Channels / GPS data) ...
...
 len+2   $xx   CRC8

Example:
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 num bytes
 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 idx bytes
         |---------------------------------------------------------------------|   No bytes in Ln   Ln=24 for type 0 / Ln=43 for type 3 (GPS data)
      |---------------------------------------------------------------------|      bytes for CRC8
h1 h2 Ln Tp Cntr  Ri Pc Ch0 Ch1  Ch2 Ch3  Ch4 Ch5  Ch6 Ch7  Ch8 Ch9  Ch10Ch11 CRC8                                                     CRC8

Known Action types in Command messages (msg type 20):
   5 - Sonar config
   9 - LED config
  10 - GPS (Ask for config)
  11 - Home altitude

Others may be from ACTION_TYPE in MissionData.java
   0 - Request
   1 - Response
   2 - Feedback
   3 - Setting CCC
   4 - Settig ROI
   6 - One key take off
   7 - Setting JOUR
   8 - Real Sense depth


}

unit SR24_dec;

{$mode objfpc}{$H+}

interface

uses
  synaser;

const
  timeout=300;
  UARTSpeed=115200;                                     {SR24 default speed}
  uartport='/dev/ttyAMA0';                              {Default port for Raspberry Pi}

  header1=$55;                                          {Message start ID}
  header2=$55;
  maxlen=$50;                                           {must be < header1 and header2 ?}
  BindMessage: array [0..10] of byte =
               (header1, header2, 8, 4, 0, 0, $42, $49, $4E, $44, $B0);
               {                len type       B    I    N    D   CRC}
  DefTelemetry: array [0..39] of byte =                 {Default telemetry message for initialization}
               (header1, header2, $26, 2, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     { 6: lat, lon, alt}
                0, 0, 0, 0, 0, 0,                       {18: vx, vy, vz }
                0, 110, 0,                              {24: nsat, voltage (16V), current}
                0, 0, 0, 0, 0, 0,                       {27: roll, pitch, yaw }
	        63, 96, 85,                             {33: motor status, IMU status, Preessure compass}
	        16, 5, 0, 40);                          {36: flight mode, vehicle type, error flags, gps_AccH}


var
  sr24ser: TBlockSerial;                                {UART access}

type
  TPayLoad = array[0..maxlen] of byte;                  {Message array}


procedure ConnectUART(port: string; speed: uint32; var UARTconnected: boolean);
procedure DisconnectUART(var UARTconnected: boolean);   {Disconnect SR24}
function  UARTcanRead: boolean;                         {Check if ready to receive}
function  UARTcanWrite: boolean;                        {Check if ready to transmit}
function  UARTreadByte: byte;                           {receive one byte}
procedure UARTwriteByte(b: byte);                       {send one byte}
function  UARTreadMsg(var data: TPayLoad): boolean;     {Read one message}
procedure UARTsendMsg(data: TPayload);                  {Send one telemetry dataset}
procedure SendBind;                                     {Send one binding message}

function  SR24_CRC8(data: TPayLoad; len: byte): byte;   {Create CRC8 checksum}
function  TestCRC8(data: TPayLoad; len: byte): boolean; inline;  {Check if dataset is valid}

function  GetFloatFromBuf(data: TPayLoad; idx: byte): single;              {Get 4 byte float as single}
function  GetIntFromBuf(data: TPayLoad; idx, len: byte): integer;          {Get len bytes as integer big endian}
procedure IntToTelemetry(var data: TPayload; w: integer; pos, len: byte);  {Convert integer to byte array}
function  GetChValue(data: TPayLoad; chnr: byte): uint16;                  {Get value from on Channel, channel no starts with 1}
function  CoordToFloat(coord: integer): single;                            {Convert integer cooerdinates to float}
function  GetGPSdata(data: TPayLoad; var lat, lon, alt: single): boolean;  {Get lat, lon and alt from GPS data}

function  VoltToByte(v: single): byte;                  {Convert volt to byte}
function  CurrentToByte(a: single): byte;               {Convert ampere to byte}
function  CoordToInt(coord: single): int32;             {Convert coordinates to integer represetation}
function  AltitudeToInt(alt: single): int32;            {Convert Altitude, 4 byte}
function  SpeedToInt(alt: single): int16;               {Speed 2 byte}
function  GetRSSI(data: TPayLoad): int16;               {Get receiver RSSI in %}

implementation

procedure ConnectUART(port: string; speed: uint32; var UARTconnected: boolean);
begin
  if not UARTconnected then begin                       {UART Tx, GPIO 14, pin 8}
    sr24ser:=TBlockSerial.Create;                       {UART Rx, GPIO 15, pin 10}
    sr24ser.Connect(port);                              {Port for Raspi: /dev/ttyAMA0}
    sr24ser.Config(speed, 8, 'N', 1, false, false);     {Config default 115200 baud, 8N1}
    UARTConnected:=true;
  end;
end;

procedure DisconnectUART(var UARTconnected: boolean);   {Disconnect and free UART}
begin
  if UARTConnected then begin
    try
      sr24ser.CloseSocket;                              {Close UART connection}
    finally
      sr24ser.Free;
    end;
    UARTConnected:=false;
  end;
end;

function UARTcanRead: boolean;                          {Wrapper for simple UART routines}
begin
  result:=sr24ser.CanRead(timeout);
end;

function UARTcanWrite: boolean;
begin
  result:=sr24ser.CanWrite(timeout);
end;

function UARTreadByte: byte;
begin
  result:=sr24ser.RecvByte(timeout);
end;

procedure UARTwriteByte(b: byte);
begin
  sr24ser.SendByte(b);
end;

function UARTreadMsg(var data: TPayLoad): boolean;      {Detect and read one message from data stream}
const
  empty: array [0..2] of byte = (255, 255, 255);        {Buffer for header bytes to check if message starts here}

var
  i, z: byte;
  buf: array[0..2] of byte;

begin
  result:=false;
  z:=0;                                                 {Counter for unsynced bytes}
  buf:=empty;                                           {Reset buffer}
  repeat
    buf[0]:=sr24ser.RecvByte(timeout);                  {read byte by byte}
    if (buf[2]=header1) and                             {check if valid message (header+plausible length)}
       (buf[1]=header2) and
       (buf[0]<maxlen) then begin
      data[0]:=buf[2];                                  {Copy header and length to message data}
      data[1]:=buf[1];
      data[2]:=buf[0];
      for i:=3 to buf[0]+2 do                           {Read the other bytes of the dataset (payload + CRC)}
        data[i]:=sr24ser.RecvByte(timeout);
      z:=0;
      result:=true;
    end else begin                                      {Shift buffer right}
      buf[2]:=buf[1];
      buf[1]:=buf[0];
      inc(z);                                           {Count bytes to prevent overflow}
    end;
  until result or                                       {Valid message but w/o CRC check}
       (z>maxlen);                                      {Too long message}
end;

procedure UARTsendMsg(data: TPayload);                  {Send one telemetry dataset}
var
  crc, i: byte;

begin
  for i:=0 to data[2]+1 do
    sr24ser.SendByte(data[i]);
  crc:=SR24_CRC8(data, data[2]);
  sr24ser.SendByte(crc);
end;

procedure SendBind;                                     {Send one BIND message}
var
  i: integer;

begin
  if sr24ser.CanWrite(timeout) then begin
    for i:=0 to 10 do begin
      sr24ser.SendByte(BindMessage[i]);
    end;
  end;
end;

function SR24_CRC8(data: TPayLoad; len: byte): byte;    {Compute CRC8 checksum}
var
  b, i: byte;

begin                                                   {Translated from ST24.c}
  result:=0;
  for i:=2 to len+1 do begin                            {i points to databyte in array}
    b:=$80;
    repeat
      if (result and $80) <>0 then begin
        result:=result shl 1;
        result:=result xor 7;
      end else
        result:=result shl 1;
      if (data[i] and b)<>0 then
        result:=result xor 7;
      b:=b shr 1;
    until b=0;
  end;
end;

function TestCRC8(data: TPayLoad; len: byte): boolean; inline;
begin
  result:=(data[len+2]=SR24_CRC8(data, len));
end;

{http://forum.lazarus-ide.org/index.php?topic=42182.0
 Direkter Typecast mit dem Zieldatentyp oder die Deklaration mittels absolute}

function GetFloatFromBuf(data: TPayLoad; idx: byte): single; {Position, Länge immer 4}
var i: byte;
    wfl: packed array[0..3] of Byte;
    wx: Single absolute wfl;

begin
  result:=0;
  for i:=0 to 3 do
    wfl[i]:=data[idx+i];                                {Get 4 byte from array}
  result:=wx;                                           {Typecast by absolute}
end;

{Integer represent byts to integer values, MSB is right (big endian)}

function GetIntFromBuf(data: TPayLoad; idx, len: byte): integer;   {len is 2, 4 or 8}
var
  i: byte;

begin
  result:=0;                                            {Lowest byte}
  for i:=len-1 downto 1 do
    result:=(result+data[idx+i]) shl 8;
  result:=result+data[idx];
  if ((data[idx+len-1] and $80)<>0) and
     (result>0) then                                    {Check if negative value}
    result:=-result;
end;

procedure IntToTelemetry(var data: TPayload; w: integer; pos, len: byte);
                                                        {Convert integer to byte array}
var
  i, x: integer;
begin
  x:=w;
  data[pos]:=x and $FF;
  for i:=1 to len-1 do begin
    x:=x shr 8;
    data[pos+i]:=x and $FF;
  end;
  if w<0 then
    data[pos+len-1]:=data[pos+len-1] or $80;
end;

function GetChValue(data: TPayLoad; chnr: byte): uint16; {Channel no from 1..12 or 1..24}
var
  n: byte;

begin
  n:=((chnr-1) div 2)*3+8;
  if (chnr and 1)=0 then begin                           {even channel no Ch0...}
    result:=lo(data[n+1])*256+data[n+2];
  end else begin                                         {uneven channel no Ch1...}
    result:=data[n]*16+hi(data[n+1]);
  end;
end;

function CoordToFloat(coord: integer): single;
begin
  result:=coord/10000000;
end;

{Get most important GPS data: Latitude, longitude, altitude (ASL)}

function GetGPSdata(data: TPayLoad; var lat, lon, alt: single): boolean;
var                                                     {Write into lat, lon and alt (ASL)}
  la, lo: integer;

begin
  result:=false;
  la:=GetIntFromBuf(data, 26, 4);
  lo:=GetIntFromBuf(data, 30, 4);
  lat:=CoordToFloat(la);
  lon:=CoordToFloat(lo);
  alt:=GetFloatFromBuf(data, 34);
  if (la<>0) or (lo<>0) then
    result:=true;
end;

function VoltToByte(v: single): byte;                   {Yuneec voltage representation}
begin
  result:=round((v-5)*10);
end;

function CurrentToByte(a: single): byte;                {Current in A}
begin
  result:=round(a/2);
end;

function CoordToInt(coord: single): int32;              {Convert coordinates from single to integer}
begin
  result:=round(coord*10000000);
end;

function AltitudeToInt(alt: single): int32;             {Convert Altitude in m to interger}
begin
  result:=round(alt*100);
end;

function SpeedToInt(alt: single): int16;                {Convert speed in m/s to integer}
begin
  result:=round(alt*100);
end;

function GetRSSI(data: TPayLoad): int16;                {Get receiver RSSI in %}
begin
  result:=round(data[6]*100/255);                       {in %}
end;

end.
