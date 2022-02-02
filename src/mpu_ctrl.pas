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

Stripped down to MPU6050
}

unit mpu_ctrl;

{$mode objfpc}{$H+}

interface

uses
   sysutils, strutils, process;

const
  MPUadr='0x68';                                   {Default address of I2C chips}

  bus='1';                                         {Used I2C bus, default}
{Read/write register MPU6050}
  rwregs=[13..16, 25..28, 35..52, 55, 56, 99..104, 106..108, 114..116];
  roregs=[53, 54, 58..96, 117];                    {Read-only registers}
  rst107=$40;                                      {Reset value for power management}

  i2cdct='i2cdetect';
  i2cget='i2cget';
  i2cset='i2cset';
  yes='-y';                                        {Disable interactive mode}
  hexidc='0x';
  hexidp='$';

function GetAdrStrMPU: boolean;                    {Check MPU address from register WHO_AM_I}
function GetReg(adr: string; r: byte): byte;       {Read byte from MPU}
function GetRegWbe(adr: string; r: byte): int16;   {Read word from MPU}
procedure SetReg(adr: string; r, v: byte);         {Write byte v to register r}
procedure MPUWakeUp;                               {Power management set to wake up}
function GetFS_SEL: byte;                          {Read scale factor for gyro (27)}
function GetAFS_SEL: byte;                         {Read scale factor for acc (28)}
function ConvTemp(temp: int16): double;            {Convert temperature}
function TempToStr: string;                        {Show chip temperature as string}
function ConvGyro(fs: byte; gy: int16): double;    {Convert gyro values according datasheet}
function ConvAcc(afs: byte; acc: int16;            {Convert acc values according datasheet}
                 mg: boolean=false): double;       {alternativ: output in mG}


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

end.
