{MPU6050 control 

 Read/write register of MPU6050 by terminal commands at Raspberry Pi
 --------------------------------------------------------------------------------

 Descrition of register
 https://invensense.tdk.com/wp-content/uploads/2015/02/MPU-6000-Register-Map1.pdf

 Preparations:
 -------------
 Enable I2C: sudo raspi-config > Interface Options > I2C > Yes
 Possibly you need user access:
 sudo usermod -aG i2c pi (pi or any other user)

 Connect MPU and check wiring (bus 1).
 pin1  Vcc
 pin3  SDA
 pin5  SCL
 pin6  GND

 Check if MPU6050 is available:	i2cdetect -y 1 > should be appear at address 0x68

 Some useful terminal commands:
 ------------------------------
 Read a byte from MPU:	i2cget -y 1 0x68 0x75 (Who am I, it's own address)
 Read a word from MPU:	i2cget -y 1 0x68 65 w (result comes as big endian)
 Write a byte to MPU register:	i2cset y 1 0x68 107 0 (wake-up command)
 Read temperature cyclic (raw):	watch -n 0.5 'i2cget -y 1 0x68 65 w'

}

unit mpu_ctrl;

{$mode objfpc}{$H+}

interface

uses
   sysutils, strutils, process;

const
  MPUadr='0x68';                                   {Default address of I2C chips}
  ISTadr='0x0E';
  ISTID= '0x10';
  HMCadr='0x1E';
  intfac='I²C';

  bus='1';                                         {Used I2C bus, default}
{Read/write register MPU6050}
  rwregs=[13..16, 25..28, 35..52, 55, 56, 99..104, 106..108, 114..116];
  roregs=[53, 54, 58..96, 117];                    {Read-only registers}
  rwIST=[10..12, 65, 66];                          {Register IST8310}
  roIST=[0, 2..9, 28, 29];
  rwHMC=[0..2];                                    {Register HMC5883}
  roHMC=[3..12];
  rst107=$40;                                      {Reset value for power management}

  i2cdct='i2cdetect';
  i2cget='i2cget';
  i2cset='i2cset';
  i2cdump='i2cdump';
  yes='-y';                                        {Disable interactive mode}
  hexidc='0x';
  hexidp='$';

function GetAdrStrMPU: boolean;                    {Check MPU address from register WHO_AM_I}
function GetAdrStrIST: boolean;                    {Check IST8310 address from register WHO_AM_I}
function GetAdrStrHMC: boolean;                    {Check HMC5883 address frm ID register A}
function GetReg(adr: string; r: byte): byte;       {Read byte from MPU}
function GetRegWbe(adr: string; r: byte): int16;   {Read word from MPU}
function GetRegWle(adr: string; r: byte): int16;   {Read word from IST little endian}
procedure SetReg(adr: string; r, v: byte);         {Write byte v to register r}
procedure MPUWakeUp;                               {Power management set to wake up}
procedure ISTreset;                                {Soft reset IST8310}
function GetFS_SEL: byte;                          {Read scale factor for gyro (27)}
function GetAFS_SEL: byte;                         {Read scale factor for acc (28)}
function ConvTemp(temp: int16): double;            {Convert temperature}
function TempToStr: string;                        {Show chip temperature as string}
function ConvGyro(fs: byte; gy: int16): double;    {Convert gyro values according datasheet}
function ConvAcc(afs: byte; acc: int16;            {Convert acc values according datasheet}
                 mg: boolean=false): double;       {alternativ: output in mG}
function afsToStr(afs: byte): string;              {Just for information about used acc scale}
function fsToStr(fs: byte): string;                {Just for information about used gyro scale}
function AdrToChip(adr: string): string;           {Find chip type on I2C bus 1}
function ScanI2C(intf: char='1'): string;          {Scan a I2C interface to find all connected chips}


implementation

function GetAdrStrMPU: boolean;                    {Check MPU address from register WHO_AM_I}
var
  s: string;

begin
  s:='';
  RunCommand(i2cdct, [yes, bus], s);
  RunCommand(i2cget, [yes, bus, MPUadr, '117'], s);
  result:=trim(s)=MPUadr;
end;

function GetAdrStrIST: boolean;                    {Check IST8310 address from register WHO_AM_I}
var
  s: string;

begin
  s:='';
  RunCommand(i2cdct, [yes, bus], s);
  RunCommand(i2cget, [yes, bus, ISTadr, '0'], s);
  result:=trim(s)=ISTID;
end;

function GetAdrStrHMC: boolean;                    {Check HMC5883 address frm ID register A}
var
  s: string;

begin
  s:='';
  RunCommand(i2cdct, [yes, bus], s);
  RunCommand(i2cget, [yes, bus, HMCadr, '0x0A'], s);
  result:=trim(s)='0x48';
end;

function GetReg(adr: string; r: byte): byte;       {Read byte from MPU}
var
  s: string;

begin
  RunCommand(i2cget, [yes, bus, adr, IntToStr(r)], s);
  s:=ReplaceText(trim(s), hexidc, hexidp);
  result:=StrToIntDef(s, $FF);
end;

function GetRegWbe(adr: string; r: byte): int16;   {Read word from MPU}
var
  s: string;
  w: int16;

begin
  RunCommand(i2cget, [yes, bus, adr, IntToStr(r), 'w'], s);
  s:=ReplaceText(trim(s), hexidc, hexidp);         {Word from i2cget is big endian}
  w:=StrToIntDef(s, $FFFF);
  result:=BEtoN(w);
end;

function GetRegWle(adr: string; r: byte): int16;   {Read word from IST little endian}
var
  s: string;

begin
  RunCommand(i2cget, [yes, bus, adr, IntToStr(r), 'w'], s);
  s:=ReplaceText(trim(s), hexidc, hexidp);
  result:=StrToIntDef(s, $FFFF);
end;

procedure SetReg(adr: string; r, v: byte);         {Write byte v to register r}
var
  s: string;

begin
  RunCommand(i2cset, [yes, bus, adr, IntToStr(r), IntToStr(v)], s);
end;

procedure MPUWakeUp;                               {Power management set to wake up}
begin
  SetReg(MPUadr, 107, 0);
end;

procedure ISTreset;                                {Soft reset IST8310}
begin
  SetReg(ISTAdr, 11, 13);                          {Control register2 Soft reset}
end;

function GetFS_SEL: byte;                          {Read scale factor for gyro}
var
  b: byte;

begin
  b:=GetReg(MPUadr, 27);                           {Gyro_CONFIG}
  result:=(b and $18) shr 3;                       {Gyro Scale 0..3}
end;

function GetAFS_SEL: byte;                         {Read scale factor for Acc}
var
  b: byte;

begin
  b:=GetReg(MPUadr, 28);                           {Acc_CONFIG}
  result:=(b and $18) shr 3;                       {Acc Scale 0..3}
end;

{Temperature in °C = (TEMP_OUT Register Value as a signed quantity)/340 + 36.53}
function ConvTemp(temp: int16): double;
begin
  result:=temp/340+36.53;
end;

function TempToStr: string;                        {Show chip temperature as string}
begin
  result:=FormatFloat('0.0', ConvTemp(GetRegWbe(MPUadr, 65)))+'°C';
end;

function ConvGyro(fs: byte; gy: int16): double;    {Convert gyro values according datasheet}
begin
  case fs of
    0: result:=gy/131;                             {+/- 250°/s}
    1: result:=gy/65.5;
    2: result:=gy/32.8;
    3: result:=gy/16.4;                            {+/- 2000 °/S}
  end;
end;

function ConvAcc(afs: byte; acc: int16;            {Convert acc values according datasheet}
                 mg: boolean=false): double;       {output in mG}
begin
  case afs of
    0: result:=acc/16384;                          {+/- 2G}
    1: result:=acc/8192;
    2: result:=acc/4096;
    3: result:=acc/2048;                           {+/- 16G}
  end;
  if mg then
    result:=result*1000;
end;

function afsToStr(afs: byte): string;              {Just for information about used acc scale}
begin
  case afs of
    0: result:='+/- 2G';
    1: result:='+/- 4G';
    2: result:='+/- 8G';
    3: result:='+/- 16G';
  end;
end;

function AdrToChip(adr: string): string;           {Find chip type on I2C bus 1}
begin
  result:='';
  if adr=MPUadr then begin
    result:='MPU6050';
    exit
  end;
  if adr=ISTadr then begin
    result:='IST8310';
    exit
  end;
  if adr=HMCadr then begin
    result:='HMC5883';
  end;
end;

function fsToStr(fs: byte): string;                {Just for information about used gyro scale}
begin
  case fs of
    0: result:='+/- 250°/s';
    1: result:='+/- 500°/s';
    2: result:='+/- 1000°/s';
    3: result:='+/- 2000°/s';
  end;
end;

function ScanI2C(intf: char=bus): string;          {Scan a I2C interface to find all connected chips}
var
  i: byte;
  s, adr: string;

begin
  result:='';
  RunCommand(i2cdct, [yes, bus], s);
  for i:=3 to 127 do begin
    adr:=hexidc+IntToHex(i, 2);                    {Test all possible addresses}
    RunCommand(i2cget, [yes, intf, adr, '0'], s);
    if pos(hexidc, s)>0 then
      result:=result+adr+' ';
  end;
  result:=trim(result);
end;

end.
