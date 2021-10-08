{GPIO control 

 Control GPIO by wiringpi
 ------------------------
 Wiringpi is a well known wrapper to control GPIO. Hopefully it is faster than using sysfs.
 wiringpi should be already installed. If not install with "sudo apt install wiringpi -y" 

 GPIO:
 https://projects.drogon.net/raspberry-pi/wiringpi/the-gpio-utility/
 https://www.ics.com/blog/gpio-programming-using-sysfs-interface

 Defaults (pwm-2chan):

 Test wiringpi in terminal:
 gpio -v
 gpio readall
 gpio export 23 out
 gpio exports
 gpio unexport 23

}

unit SR24_wripi;

{$mode objfpc}{$H+}

interface

uses
  sysutils, process;

const
  gpiocmd='gpio';
  write='write';
  minusg='-g';
  fSysStart='export';                                    {Activate GPIO pin in file system}
  fSysStop='unexport';                                   {Deactivate GPIO pin in file system}


function ActivatePWMChannel(GPIOnr: string): byte;       {results PWM status after activation}
procedure DeactivatePWM;                                 {Deactivate all PWM channels}
procedure DeactivateGPIO(GPIOnr: string);                {Deactivate and close GPIO pin}
procedure SetPWMChannel(const chan: byte; freq, cycle: uint32);
procedure SetPWMCycle(const chan: byte; cycle: uint32);  {Set duty cycle in micro seconds}
function ActivateGPIO(GPIOnr: string): boolean;          {Open GPIO port as Out/Low as default}
procedure SetGPIO(GPIOnr: string; Gbit: char = '0');     {Output on one GPIO out-pin}
function RunWiringPiCmd(parameter: array of string): string;  {Wrapper for any wiringpi command}

implementation

function ActivatePWMChannel(GPIOnr: string): byte;  {Activate one (PWM0) or both PWM channels}
var
  tout: string;

begin

end;

procedure DeactivatePWM;                                 {For compatibility, in this case deactivate all}
var
  tout: string;

begin
  RunCommand(gpiocmd, ['unexportall'], tout);
end;

function ActivateGPIO(GPIOnr: string): boolean;          {Open GPIO port as Out/Low as default}
var
  tout: string;

begin
  RunCommand(gpiocmd, [fSysStart, GPIOnr, 'out'], tout);
  result:=RunCommand(gpiocmd, [minusg, write, GPIOnr, '0'], tout);
end;

procedure DeactivateGPIO(GPIOnr: string);                {Deactivate and close GPIO pin}
var
  tout: string;

begin
  RunCommand(gpiocmd. [fSysStop, GPIOnr], tout);
end;

procedure SetPWMChannel(const chan: byte; freq, cycle: uint32; revers: boolean = true);
var
  tout: string;

begin

end;

procedure SetPWMCycle(const chan: byte; cycle: uint32);  {Set duty cycle in micro sec}
var
  tout: string;

begin

end;

procedure SetGPIO(GPIOnr: string; Gbit: char = '0');     {Output on one GPIO out-pin}
bvar
  tout: string;

begin
  RunCommand(gpiocmd, [minusg, write, GPIOnr, Gbit], tout);
end;

function RunWiringPiCmd(parameter: array of string): string;  {Wrapper for any wiringpi command}
begin
  result:='';
  RunCommand(gpiocmd, parameter, result);
end;


(*
Read something:
RunCommand('gpio', ['-g', 'mode', '23', 'ouput'], s);
RunCommand('gpio', ['-g', 'read', '23'], s);
Memo1.Lines.Add(s);

*)

end.
