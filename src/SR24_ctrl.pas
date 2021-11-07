{GPIO control 

 Control GPIO of Raspberry Pi by sysfs
 -------------------------------------

 Basic GPIO control using the sysfs interface is slower than writing into memory
 but easier to program.

 PWM:
 https://github.com/raspberrypi/firmware/blob/master/boot/overlays/README

 GPIO:
 https://www.ics.com/blog/gpio-programming-using-sysfs-interface

 Preparations:
 -------------
 To activate the 2 PWM channels with default pins, add
     dtoverlay=pwm-2chan,pin=18,func=2,pin2=13,func2=4
 to /boot/config.txt and reboot.

 Defaults (pwm-2chan):
 GPIO18 (pin12) PWM0
 GPIO13 (pin33) PWM1

 Check if pwm chip is active:
  cd /sys/class/pwm/pwmchip0/

 Set up to test PWM:
  echo 0 > export   (deactivate with echo 0 > unexport)
  cd pwm0
  echo 10000 > period    (100kHz)
  echo 5000 > duty_cycle (length pulse --> 1:1)
  echo 1 > enable
}

unit SR24_ctrl;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
    baseunix,
  {$EndIf}
    sysutils;

const
  pathPWM='/sys/class/pwm/pwmchip0/';                    {Path to HW PWM in file system}
  pathGPIO='/sys/class/gpio/';                           {Path to GPIO pins}
  fgpio='gpio';                                          {Representation pin in file system, add GPIO number}
  PWMchan0='pwm0/';
  PWMchan1='pwm1/';
  fSysStart='export';                                    {Activate GPIO pin in file system}
  fSysStop='unexport';                                   {Deactivate GPIO pin in file system}
  fDirection='/direction';
  fValue='/value';
  fperiod='period';                                      {in ns --> 1/f in Ghz}
  fCycle='duty_cycle';                                   {in ns must be lower than period}
  fRevers='polarity';
  fEnable='enable';                                      {1 enable, 0 disable}
  keyRevers='inversed';
  keyNoRevers='normal';
  keyIn='in';
  keyOut='out';
  GPIOhigh='1';
  GPIOlow='0';

  ziff=['0'..'9'];                                       {Valid digits in a string}
  GPIOinvalid=28;                                        {GPIOnr must be lower than that}
  GPIOnodat=' ';                                         {Could not read data from GPIO port}

  waitfs=100;                                            {Wait for file system to create GPIO system files}


type
  SysData=string[16];                                    {16 is maximum length to send a string}

function  WriteSysFile(filename: string; const value: SysData): boolean;
function  ReadSysFile(filename: string): char;           {Read '0' or '1' from GPIO}
function  ValidGPIOnr(pionr: byte): boolean;             {Check if GPIOnr is in range}
function  ExtractGPIOnr(gpn: string): byte;              {GPIO number from name}

function  ActivatePWMChannel(both: boolean=true): byte;  {results PWM status after activation}
function  PWMStatus: byte;                               {0: PWM not activated at boot,
                                                          1: No channel active
                                                          3: Channel 0 only,
                                                          7: both 0 and 1 active}
procedure SetPWMChannel(const PWMnr: byte; peri: uint32;
                        cycle: uint64; revers: boolean = true);
procedure SetPWMCycle(const PWMnr: byte; cycle: uint64); {Set duty cycle in ns}
procedure DeactivatePWM;                                 {Deactivate all PWM channels}

{Write data to PWM channel: PWM channel 0 or 1, PWM period in µs, cycle in ns, inversed-Default not}
function  ActivateGPIO(GPIOnr, dir: byte): boolean;      {Open GPIO port as Out/Low as default, dir=1 means input}
procedure SetGPIO(GPIOnr: byte; Gbit: char=GPIOlow);     {Output on one GPIO out-pin}
function  GetGPIO(GPIOnr: byte): char;                   {Read GPIO out-pin}
procedure DeactivateGPIO(GPIOnr: byte);                  {Deactivate and close GPIO pin}

implementation

function WriteSysFile(filename: string; const value: SysData): boolean;
{$IFDEF UNIX}
var
  f: cint;
{$EndIf}

begin
  result:=false;
  {$IFDEF UNIX}
    f:=fpOpen(filename, o_wronly);                       {Open file to write a string}
    if f>0 then begin
      try
        if length(value)=fpWrite(f, value[1], length(value)) then
          result:=true;
      finally
        fpClose(f);
      end;
    end;
  {$EndIf}
end;

function ReadSysFile(filename: string): char;            {Read '0' or '1' from GPIO}
{$IFDEF UNIX}
var
  f: cint;
{$EndIf}

begin
  result:=GPIOnodat;                                     {Space as default = got nothing}
  {$IFDEF UNIX}
    f:=fpOpen(filename, o_rdonly);                       {Open file to read a string}
    if f>0 then begin
      try
        fpRead(f, result, 1);
      finally
        fpClose(f);
      end;
    end;
  {$EndIf}
end;

function ValidGPIOnr(pionr: byte): boolean;              {Check if GPIOnr is in range}
begin
  result:=((pionr<GPIOinvalid) and (pionr>1));
end;

function ExtractGPIOnr(gpn: string): byte;               {GPIO number from name}
var
  i: integer;
  s: string;

begin
  s:='';
  result:=GPIOinvalid;                                   {invalid GPIO port}
  for i:=1 to length(gpn) do
    if gpn[i] in ziff then
      s:=s+gpn[i];
  i:=StrToInt(s);
  if ValidGPIOnr(i) then                                 {GPIO 2 .. GPIO 27}
    result:=i;
end;

function ActivatePWMChannel(both: boolean=true): byte;   {Activate one or both PWM channels}
begin
  WriteSysFile(pathPWM+fSysStart, '0');                  {Activate PWM0 by default}
  if both then
    WriteSysFile(pathPWM+fSysStart, '1');                {PWM1 too}
  sleep(waitfs);                                         {Wait for file is written to GPIO}
  result:=PWMstatus;
end;

function PWMStatus: byte;                                {result: 0, 1, 3 or 7}
begin
  result:=0;                                             {HW-PWM not activated at boot}
  if DirectoryExists(pathPWM) then
    result:=1;                                           {1: System file created}
  if DirectoryExists(pathPWM+PWMchan0) then
    result:=result or 2;                                 {3: PWM channel 0 OK}
  if DirectoryExists(pathPWM+PWMchan1) then
    result:=result or 4;                                 {7: PWM channel 0 and 1 OK}
end;

{Write data to PWM channel: PWM channel 0 or 1, PWM period in µs, cycle in ns, inversed-Default not}
procedure SetPWMChannel(const PWMnr: byte; peri: uint32;
                        cycle: uint64; revers: boolean = true);
var
  speriod, scycle: SysData;

begin
  speriod:=IntToStr(peri*1000);                          {Convert from µs to ns, default: 20000}
  scycle:=IntToStr(cycle);                               {Cycle in ns}
  case PWMnr of
    0: begin
         WriteSysFile(pathPWM+PWMchan0+fperiod, speriod);
         WriteSysFile(pathPWM+PWMchan0+fcycle, scycle);
         if revers then
           WriteSysFile(pathPWM+PWMchan0+frevers, keyrevers);
         WriteSysFile(pathPWM+PWMchan0+fenable, GPIOhigh);
       end;
    1: begin
         WriteSysFile(pathPWM+PWMchan1+fperiod, speriod);
         WriteSysFile(pathPWM+PWMchan1+fcycle, scycle);
         if revers then
           WriteSysFile(pathPWM+PWMchan1+frevers, keyrevers);
         WriteSysFile(pathPWM+PWMchan1+fenable, GPIOhigh);
       end;
  end;
end;

procedure SetPWMCycle(const PWMnr: byte; cycle: uint64);  {Set duty cycle in ns}
var
  scycle: SysData;

begin
  scycle:=IntToStr(cycle);
  case PWMnr of
    0: WriteSysFile(pathPWM+PWMchan0+fcycle, scycle);
    1: WriteSysFile(pathPWM+PWMchan1+fcycle, scycle);
  end;
end;

procedure DeactivatePWM;                                 {Deactivate and close HW-PWM channels}
var
  status: byte;

begin
  status:=PWMstatus;
  if status=3 then begin
    WriteSysFile(pathPWM+PWMchan0+fenable, GPIOlow);
    WriteSysFile(pathPWM+fSysStop, GPIOlow);
    exit;
  end;
  if status=7 then begin
    WriteSysFile(pathPWM+PWMchan0+fenable, GPIOlow);     {disable PWM}
    WriteSysFile(pathPWM+PWMchan1+fenable, GPIOlow);     {Switch off channel 0}
    WriteSysFile(pathPWM+fSysStop, GPIOlow);
    WriteSysFile(pathPWM+fSysStop, GPIOhigh);            {Switch off channel 1}
  end;
end;

function ActivateGPIO(GPIOnr, dir: byte): boolean;       {Open GPIO port as Out and Low as default}
var
  pionr, dr: string;

begin                                                    {dir=1 .. input, dir=0 .. output}
  result:=false;
  if dir=1 then
    dr:=keyIn
  else
    dr:=keyOut;
  if GPIOnr<GPIOinvalid then begin                       {Activate pins up to GPIO27}
    pionr:=IntToStr(GPIOnr);
    WriteSysFile(pathGPIO+fSysStart, pionr);
    sleep(waitfs);                                       {Wait for directory is written to GPIO}
    if DirectoryExists(pathGPIO+fgpio+pionr) then begin
      WriteSysFile(pathGPIO+fgpio+pionr+fDirection, dr);
      if dir<>1 then
        SetGPIO(GPIOnr, '0');                            {Send default 0}
      result:=true;
    end;
  end;
end;

procedure SetGPIO(GPIOnr: byte; Gbit: char = GPIOlow);   {Send value '0' or '1' to out-pin}
begin
  if GPIOnr<GPIOinvalid then
    WriteSysFile(pathGPIO+fgpio+IntToStr(GPIOnr)+fValue, Gbit);
end;

function GetGPIO(GPIOnr: byte): char;                    {Get value '0' or '1' from any pin}
begin
  if GPIOnr<GPIOinvalid then
    result:=ReadSysFile(pathGPIO+fgpio+IntToStr(GPIOnr)+fValue);
end;

procedure DeactivateGPIO(GPIOnr: byte);                  {Deactivate and close GPIO pin}
begin
  if GPIOnr<GPIOinvalid then
    WriteSysFile(pathGPIO+fSysStop, IntToStr(GPIOnr));
end;


end.
