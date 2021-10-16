{GPIO control 

 Control GPIO by sysfs
 ---------------------

 Basic GPIO control using the sysfs interface is slower than writing into memory
 but easier to program.

 PWM:
 https://github.com/raspberrypi/firmware/blob/master/boot/overlays/README

 GPIO:
 https://www.ics.com/blog/gpio-programming-using-sysfs-interface

 Defaults (pwm-2chan):
 GPIO18 (pin12) PWM0
 GPIO13 (pin33) PWM1

 To activate the 2 PWM channels with default pins, add
     dtoverlay=pwm-2chan,pin=18,func=2,pin2=13,func2=4
 into /boot/config.txt and reboot.

 check if pwm chip is active:
 cd /sys/class/pwm/pwmchip0/

 echo 0 > export
 echo 1 > export (deactivate with echo 0 > unexport)
 There now should be pwm0 and pwm1

 Set up to test PWM:
 cd pwm0
 echo 10000 > period    (100kHz)
 echo 5000 > duty_cycle (length pulse --> 1:1)
 echo 1 > enable

}

unit SR24_ctrl;

{$mode objfpc}{$H+}

interface

uses
  sysutils, baseunix;

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

  ziff=['0'..'9'];
  GPIOinvalid=28;

  waitfs=50;                                             {Wait for file system to create GPIO system file}


type
  SysData=string[16];                                    {16 is maximum length to send a string}

function  WriteSysFile(filename: string; const value: SysData): boolean;
function  ExtractGPIOnr(gpn: string): byte;              {GPIO number from name}
function  PWMStatus: byte;                               {0: PWM not activated,
                                                          3: Channel 0,
                                                          7: both 0 and 1 active}
function  ActivatePWMChannel(GPIOnr: string): byte;      {results PWM status after activation}
procedure DeactivatePWM;                                 {Deactivate all PWM channels}
procedure DeactivateGPIO(GPIOnr: byte);                  {Deactivate and close GPIO pin}
{Write data to PWM channel: PWM channel 0 or 1, PWM period in micro sec, cycle in ns, inversed-Default not}
procedure SetPWMChannel(const PWMnr: byte; freq: uint32; cycle: uint64; revers: boolean = true);
procedure SetPWMCycle(const PWMnr: byte; cycle: uint64); {Set duty cycle in ns}
function  ActivateGPIO(GPIOnr: byte): boolean;           {Open GPIO port as Out/Low as default}
procedure SetGPIO(GPIOnr: byte; Gbit: char = '0');       {Output on one GPIO out-pin}

implementation

function WriteSysFile(filename: string; const value: SysData): boolean;
var
  f: cint;

begin
  result:=false;
  f:=fpOpen(filename, o_wronly);                         {Open file to write a string}
  if f>0 then begin
    try
      if length(value)=fpWrite(f, value[1], length(value)) then
        result:=true;
    finally
      fpClose(f);
    end;
  end;
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
  if (i>4) and (i<GPIOinvalid) then                      {GPIO 5 .. GPIO 27}
    result:=i;
end;

function PWMStatus: byte;                                {result: 0, 3 or 7}
begin
  result:=0;
  if DirectoryExists(pathPWM) then
    result:=1;                                           {System file created}
  if DirectoryExists(pathPWM+PWMchan0) then
    result:=result or 2;                                 {PWM channel 0 OK}
  if DirectoryExists(pathPWM+PWMchan1) then
    result:=result or 4;                                 {PWM channel 1 OK}
end;

function ActivatePWMChannel(GPIOnr: string): byte;       {Activate one (0) or both PWM channels}
var
  status: byte;

begin
  status:=PWMstatus;                                     {Check wich channels are available}
  case status of
    1: begin
         WriteSysFile(pathPWM+fSysStart, '0');
         if GPIOnr='2' then
           WriteSysFile(pathPWM+fSysStart, '1');
       end;
    3: if GPIOnr='2' then
         WriteSysFile(pathPWM+fSysStart, '1');
  end;
  sleep(waitfs);                                         {Wait for file system reaction}
  result:=PWMstatus;
end;

procedure DeactivatePWM;                                 {Deactivate and close HW-PWM channels}
var
  status: byte;

begin
  status:=PWMstatus;
  if status=3 then begin
    WriteSysFile(pathPWM+PWMchan0+fenable, '0');
    WriteSysFile(pathPWM+fSysStop, '0');
    exit;
  end;
  if status=7 then begin
    WriteSysFile(pathPWM+PWMchan0+fenable, '0');         {disable PWM}
    WriteSysFile(pathPWM+PWMchan1+fenable, '0');         {Switch off channel 0}
    WriteSysFile(pathPWM+fSysStop, '0');
    WriteSysFile(pathPWM+fSysStop, '1');                 {Switch off channel 1}
  end;
end;

procedure DeactivateGPIO(GPIOnr: byte);                  {Deactivate and close GPIO pin}
begin
  if GPIOnr<GPIOinvalid then
    WriteSysFile(pathGPIO+fSysStop, IntToStr(GPIOnr));
end;

function ActivateGPIO(GPIOnr: byte): boolean;            {Open GPIO port as Out/Low as default}
var
  gpn: string;

begin
  if GPIOnr<GPIOinvalid then begin
    gpn:=IntToStr(GPIOnr);
    result:=DirectoryExists(pathGPIO+fgpio+gpn);
    if not result then begin
      WriteSysFile(pathGPIO+fSysStart, gpn);
      sleep(waitfs);
    end;
    if DirectoryExists(pathGPIO+fgpio+gpn) then begin
      WriteSysFile(pathGPIO+fgpio+gpn+fDirection, 'out');
      SetGPIO(GPIOnr);                                   {Send default 0}
      result:=true;
    end;
  end;
end;

procedure SetGPIO(GPIOnr: byte; Gbit: char = '0');       {Send value 0 or 1 to out-pin}
begin
  if GPIOnr<GPIOinvalid then
    WriteSysFile(pathGPIO+fgpio+IntToStr(GPIOnr)+fValue, Gbit);
end;

{Write data to PWM channel: PWM channel 0 or 1, PWM period in micro sec, cycle in ns, inversed-Default not}
procedure SetPWMChannel(const PWMnr: byte; freq: uint32; cycle: uint64; revers: boolean = true);
var
  speriod, scycle: SysData;

begin
  speriod:=IntToStr(freq*1000);                          {Convert from micro to ns, default: 20000}
  scycle:=IntToStr(cycle);                               {Cycle in ns}
  case PWMnr of
    0: begin
         WriteSysFile(pathPWM+PWMchan0+fperiod, speriod);
         WriteSysFile(pathPWM+PWMchan0+fcycle, scycle);
         if revers then
           WriteSysFile(pathPWM+PWMchan0+frevers, keyrevers);
         WriteSysFile(pathPWM+PWMchan0+fenable, '1');
       end;
    1: begin
         WriteSysFile(pathPWM+PWMchan1+fperiod, speriod);
         WriteSysFile(pathPWM+PWMchan1+fcycle, scycle);
         if revers then
           WriteSysFile(pathPWM+PWMchan1+frevers, keyrevers);
         WriteSysFile(pathPWM+PWMchan1+fenable, '1');
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

end.
