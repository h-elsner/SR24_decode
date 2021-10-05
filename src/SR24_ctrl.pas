{GPIO control 

 Read data from SR24
 -------------------

 Uses non-standard package "pascalio", download from https://github.com/SAmeis/pascalio
 Then open, compile package file pascalio.lpk and Add to project.

 PWM:
 https://github.com/raspberrypi/firmware/blob/master/boot/overlays/README

 Defaults (pwm-2chan):
 GPIO18 (pin12) PWM0
 GPIO13 (pin33) PWM1

 To activate the 2 PWM channels with default pins, add
     dtoverlay=pwm-2chan,pin=18,func=2,pin2=13,func2=4
 to /boot/config.txt and reboot.

 check if pwm chip is active:
 cd /sys/class/pwm/pwmchip0/

 echo 0 > export
 echo 1 > export (deactivate with echo 0 > unexport)
 There now should be pwm0 and pwm1

 Set up to test PWM:
 cd pwm0
 echo 10000 > period    (10kHz)
 echo 5000 > duty_cycle (length pulse --> 1:1)
 echo 1 > enable


}

unit SR24_ctrl;

{$mode objfpc}{$H+}

interface

uses
  sysutils, baseunix;

const
  pathPWM='/sys/class/pwm/pwmchip0/';
  fPWMstart='export';      {enter channel number 0 or 1}
  fPWMstop='unexport';     {enter channel number 0 or 1}
  PWMchan0='pwm0/';
  PWMchan1='pwm1/';
  fperiod='period';        {in ns --> 1/f in Ghz}
  fCycle='duty_cycle';     {in ns must be lower than period}
  fRevers='polarity';
  fEnable='enable';         {1 enable, 0 disable}
  keyRevers='inversed';
  keyNoRevers='normal';


type
  PWMdata=string[16];     {Maximum length to send a string}

function WritePWMFile(filename: string; const value: PWMdata): boolean;
function PWMStatus: byte;                                {0: PWM not activated,
                                                          3: Channel 0,
                                                          7: both 0 and 1 }
function ActivatePWMChannels(both: boolean=true): byte;  {results PWM status after activation}
procedure DeactivatePWM;                                 {Deactivate all PWM channels}
procedure SetPWMChannel(chan: byte; freq, cycle: single);
procedure SetPWMCycle(chan: byte; cycle: single);

implementation

function WritePWMFile(filename: string; const value: PWMdata): boolean;
var
  f: cint;

begin
  result:=false;
  f:=fpOpen(filename, o_wronly);
  if f>0 then begin
    if length(value)=fpWrite(f, value[1], length(value)) then
      result:=true;
    fpClose(f);
  end;
end;

function PWMStatus: byte;                            {result: 0, 3 or 7}
begin
  result:=0;
  if DirectoryExists(pathPWM) then
    result:=1;
  if DirectoryExists(pathPWM+PWMchan0) then
    result:=result or 2;
  if DirectoryExists(pathPWM+PWMchan1) then
    result:=result or 4;
end;

function ActivatePWMChannels(both: boolean=true): byte;
var
  status: byte;

begin
  status:=PWMstatus;
  case status of
    1: begin
         WritePWMFile(pathPWM+fPWMstart, '0');
         if both then
           WritePWMFile(pathPWM+fPWMstart, '1');
       end;
    3: if both then
         WritePWMFile(pathPWM+fPWMstart, '1');
  end;
  result:=PWMstatus;
end;

procedure DeactivatePWM;
var
  status: byte;

begin
  status:=PWMstatus;
  if status=3 then begin
    WritePWMFile(pathPWM+PWMchan0+fenable, '0');
    WritePWMFile(pathPWM+fPWMstop, '0');
    exit;
  end;
  if status=7 then begin
    WritePWMFile(pathPWM+PWMchan0+fenable, '0');  {disable PWM}
    WritePWMFile(pathPWM+PWMchan1+fenable, '0');  {Switch off channel 0}
    WritePWMFile(pathPWM+fPWMstop, '0');
    WritePWMFile(pathPWM+fPWMstop, '1');          {Switch off channel 1}
  end;
end;

procedure SetPWMChannel(chan: byte; freq, cycle: single);
var
  speriod, scycle: PWMdata;

begin
  speriod:=IntToStr(round(freq));
  scycle:=IntToStr(round(cycle));
  case chan of
    0: begin
         WritePWMFile(pathPWM+PWMchan0+fperiod, speriod);
         WritePWMFile(pathPWM+PWMchan0+fcycle, scycle);
         WritePWMFile(pathPWM+PWMchan0+fenable, '1');
       end;
    1: begin
         WritePWMFile(pathPWM+PWMchan1+fperiod, speriod);
         WritePWMFile(pathPWM+PWMchan1+fcycle, scycle);
         WritePWMFile(pathPWM+PWMchan1+fenable, '1');
       end;
  end;
end;

procedure SetPWMCycle(chan: byte; cycle: single);
var
  scycle: PWMdata;

begin
  scycle:=IntToStr(Round(cycle));
  case chan of
    0: WritePWMFile(pathPWM+PWMchan0+fcycle, scycle);
    1: WritePWMFile(pathPWM+PWMchan1+fcycle, scycle);
  end;
end;

end.
