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

  waitfs=50;                                             {Wait for file system to create GPIO system file}


type
  SysData=string[16];                                    {16 is maximum length to send a string}

function WriteSysFile(filename: string; const value: SysData): boolean;
function PWMStatus: byte;                                {0: PWM not activated,
                                                          3: Channel 0,
                                                          7: both 0 and 1 }
function ActivatePWMChannel(GPIOnr: string): byte;       {results PWM status after activation}
procedure DeactivatePWM;                                 {Deactivate all PWM channels}
procedure DeactivateGPIO(GPIOnr: string);                {Deactivate and close GPIO pin}
procedure SetPWMChannel(const chan: byte; freq, cycle: uint32; revers: boolean = true);
procedure SetPWMCycle(const chan: byte; cycle: uint32);  {Set duty cycle in ns}
function ActivateGPIO(GPIOnr: string): boolean;          {Open GPIO port as Out/Low as default}
procedure SetGPIO(GPIOnr: string; Gbit: char = '0');     {Output on one GPIO out-pin}

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

procedure DeactivateGPIO(GPIOnr: string);                {Deactivate and close GPIO pin}
begin
  WriteSysFile(pathGPIO+fSysStop, GPIOnr);
end;

function ActivateGPIO(GPIOnr: string): boolean;          {Open GPIO port as Out/Low as default}
begin
  result:=DirectoryExists(pathGPIO+fgpio+GPIOnr);
  if not result then begin
    WriteSysFile(pathGPIO+fSysStart, GPIOnr);
    sleep(waitfs);
  end;
  if DirectoryExists(pathGPIO+fgpio+GPIOnr) then begin
    WriteSysFile(pathGPIO+fgpio+GPIOnr+fDirection, 'out');
    SetGPIO(GPIOnr);                                     {Send default 0}
    result:=true;
  end;
end;

procedure SetGPIO(GPIOnr: string; Gbit: char = '0');     {Send value 0 or 1 to out-pin}
begin
  WriteSysFile(pathGPIO+fgpio+GPIOnr+fValue, Gbit);
end;

procedure SetPWMChannel(const chan: byte; freq, cycle: uint32; revers: boolean = true);
var
  speriod, scycle: SysData;

begin
  speriod:=IntToStr(freq*1000);                          {Convert from micro to ns, default: 20000}
  scycle:=IntToStr(cycle);                               {Cycle in ns}
  case chan of
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

procedure SetPWMCycle(const chan: byte; cycle: uint32);  {Set duty cycle in micro sec}
var
  scycle: SysData;

begin
  scycle:=IntToStr(cycle);
  case chan of
    0: WriteSysFile(pathPWM+PWMchan0+fcycle, scycle);
    1: WriteSysFile(pathPWM+PWMchan1+fcycle, scycle);
  end;
end;

end.
