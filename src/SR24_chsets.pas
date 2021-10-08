{Channel settings for ST10 or ST16

 Set relationship between channels and servo
 -------------------------------------------
 - assign channel to servo number (6 servos)
 - set min, max and neutral per servo, deactivated gets neutral from pitch  
 - set up, middle, down per switch (5 switches)
 - set push button (mixed to thr), default=0

Set unused servos and button to neutral, default 2048

}

unit SR24_chsets;

{$mode objfpc}{$H+}

interface

//uses
//  sysutils;

const
  filename_settings='rc_settings.set';

{Defaults from Yuneec ST16}
  stkang =2184;                                         {switch tilt angle, 10%}
  stkntrl=2048;                                         {neutral; 0%}
  stkdown=683;                                          {-100%}
  stkup  =3412;                                         {+100%}
  stkmax =4095;                                         {+150%}
  stkmin=0;                                             {-150%}
  m40val=1502;                                          {-40% Pan controllable}
  m45val=1433;                                          {-40% Pan Team mode}
  p50val=2730;                                          {+50%}
  m50val=1365;                                          {-50%}

{Servo Settings}
  sv1='Servo 1';
  sv2='Servo 2';
  sv3='Servo 3';
  sv4='Servo 4';
  sv5='Servo 5';
  sv6='Servo 6';

  chnr=  'Channel'
  svmin= 'Minimum';
  svmax= 'Maximum';
  svntrl='Neutral';

{Switches}
  sw1='Switch 1';
  sw2='Switch 2';
  sw3='Switch 3';
  sw4='Switch 4';
  sw5='Switch 5';

{Start/stop}
  abtn='Start/stop';

  swup=  'Upper  position';
  msmid= 'Middle position';
  swdown='Lower  position';
  actv=  'Active';

  comment='#';
  ObjID1= '[';
  ObjID2= ']';

function LoadSettingsFile: boolean;

function StkToProz(const w: uint16): int16;             {Stick Position to percent}

implementation

procedure SetDefaults;
begin

end;

procedure WriteSettings;
begin

end;

function LoadSettingsFile: boolean;  {Check if file available and load it}
begin
  result:=false;
end;

function StkToProz(const w: uint16): int16;             {Stick Position to percent}
begin
  result:=round(w/stkmax*300)-150;
end;

end.
