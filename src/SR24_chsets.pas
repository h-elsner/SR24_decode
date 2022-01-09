{Channel settings for ST10 or ST16

 Set relationship between channels and servo
 -------------------------------------------
 - Common values and correction/conversion factors
 - assign channel to servo number (6 servos)
 - assign servo number to PWM or GPIO (6 servos)
 - set min, max and neutral per servo
 - set up, middle, down, GPIO port per switch (5 switches)
 - set push button (mixed to thr), default=0

Set unused servos and button to neutral, default values

  PWM 0     Hardware PWM0 at GPIO 18, pin 12
  PWM 1     Hardware PWM1 at GPIO 13, pin 33

  GPIO 5    Pin 29
  GPIO 6    Pin 31
  GPIO 12   Pin 32
  GPIO 16   Pin 36
  GPIO 17   Pin 11
  GPIO 22   Pin 15
  GPIO 23   Pin 16
  GPIO 24   Pin 18
  GPIO 25   Pin 22
  GPIO 26   Pin 37
  GPIO 27   Pin 13

  Keep the other GPIO pins free for sensors.

}

unit SR24_chsets;

{$mode objfpc}{$H+}

interface

uses
  sysutils, classes;

type
  TSettings = array[0..12, 0..5] of uint16;             {Channel settings array: servos, values}

const
  filename_settings='rc_settings.set';

{Defaults from Yuneec ST16 --> see Q500log2kml manual}
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

  svx= 'Servo';
  swx= 'Switch';
  abtn='Start/stop';                                    {Start button mixed in channel x}

  voltID='Voltage';
  warn1= 'Warn 1';
  warn2= 'Warn 2';
  tasID= 'Speed';
  OffKey='Shutdown';
  aux5=  'Aux 5';
  pwmcycle='PWM cycle';
  pwmrev=  'PWM reverse';
  logging= 'Logging';

  chnr=  'Channel';
  svmin= 'Minimum';
  svmax= 'Maximum';
  svntrl='Neutral';

  swup=  'Upper  position';
  swmid= 'Middle position';
  swdown='Lower  position';
  actv=  'Active';
  pio=   'GPIOnum';
  pio2=  'GPIOnum 2';                                   {Two GPIO ports for 3-way switches}

  comment='#';
  assgn=  '=';                                          {Separator between ID and value}
  ObjID1= '[';
  ObjID2= ']';
  lzch=   ' ';
  notused=88;                                           {Set as GPIOnr for unused channels}

  DefaultSettings: TSettings =  {Correction factors for analog input: voltage, GPIOnr Warn1, GPIOnr Warn2, Speed, Shutdown, Aux5}
                                ((1, notused, notused, 1, notused, 1000),
                                {Six servos: channel, min, neutral, max, GPIOnr, PWM reverted (1) or GPIOnr2}
                                (1, 683, 2048, 3412, notused, 0), (2, 1100, 1500, 1900, 1, 0),
                                (3, 1100, 1500, 1900, 0, 0), (4, 683, 2048, 3412, notused, 0),
                                (7, 683, 2048, 3412, notused, 0), (8, 683, 2048, 3412, notused, 0),
                                {Five switches: channel, up, middle, down, GPIOnr, GPIOnr2}
                                (5, 3412, 2048, 683, notused, notused), (9, 2184, 2184, 3412, notused, notused),
                                (10, 683, 1502, 3412, 16, 17),(11, 4095, 4095, 0, 26, notused),
                                (12, 4095, 4095, 0, notused, notused),
                                {start/stop button: channel, active, GPIOnr and PWM cycle for all servos, n/a, logging}
                                (1, 0, notused, 20000, 0, 0));

function  GetSettingsFile(bak: boolean=false): string;  {Get path and file name to settings}
function  GetIDvalue(s, id: string): string;            {Get parameter value belonging to ID}
function  GetControlType(idx: byte): byte;              {Check if servos (1) or switches (7)}
procedure SettingsToText(const sets: TSettings;
                         var liste: TStringlist);       {Create text file from settings}
procedure WriteDefaultsSettings;                        {Array DefaultSetting into text file}
function  ReadSettingsList(inlist: TStringList): TSettings;
procedure ReadSettings(var sets: TSettings);            {Fill settings array from file}

function  StkToPWM(sets: TSettings; servo: byte;        {Analog stick position to PWM in ns}
                  value: uint16): uint64;
function  SwitchPos(sets: TSettings; switch: byte;      {Position up..1, middle..2, down..3}
                   value: uint16; defaultpos: byte=2): byte;
function  StartStop(sets: TSettings; channel: byte;
                   value: uint16): boolean;             {Start/stop active or not}
function  StkToProz(const w: uint16): int16;            {Stick Position to percent}
function  VoltToTelemetry(sets: TSettings;              {Voltage to Yuneec format with correction factor}
                         volt: uint16): byte;

implementation

{Find out what type of control it is depending on index.
 Output:  0     Correction factors, warnings
          1..6  Servos 1-6
          7..11 Switches 1-5, 2-way or 3-way.
         12..Start/stop button and some common values
        >12..notused index, SW fault

Channel <> Index (TSettings array) in case of ST10/ST16 standard assignment
Thr        1 - 1
Roll       2 - 2
Pitch      3 - 3
Yaw        4 - 4
f mode     5 - 7
(RTH)      6 - n/a
Cam Tilt   7 - 5
--------------------------------------
Cam pan    8 - 6    +ST12     for ST16
Tilt mode  9 - 8
Pan mode  10 - 9
Gear sw   11 - 10   +ST12
Aux btn   12 - 11
}

function GetControlType(idx: byte): byte;               {Check if servos (1) or switches (7)}
begin
  case idx of
    0:      result:=0;                                  {Correction factors}
    1..6:   result:=1;                                  {Servos}
    7..11:  result:=7;                                  {Switches}
  else
    result:=idx;
  end;
end;

function GetSettingsFile(bak: boolean=false): string;   {Get path and file name to settings file}
begin
  result:=ExtractFilePath(paramstr(0))+filename_settings;
  if bak then
    result:=ChangeFileExt(result, '.bak');
end;

function GetIDvalue(s, id: string): string;             {Get parameter value belonging to ID}
begin
  result:='';
  if trim(s.Split([assgn])[0])=id then
    result:=(trim(s.Split([assgn])[1]));
end;

{Read the settings array and write the related text file.
 This is used to create default settings file and for tests}

procedure SettingsToText(const sets: TSettings;
                         var liste: TStringlist);       {Create text file from settings}
var
  i: integer;
  rv: boolean;

begin
  liste.Add(comment+' Common settings');
  liste.Add(pwmcycle+lzch+assgn+lzch+IntToStr(sets[12, 3]));
  liste.Add(logging+lzch+assgn+lzch+IntToStr(sets[12, 5]));
  liste.Add(warn1+lzch+assgn+lzch+IntToStr(sets[0, 1]));     {Voltage warning 1}
  liste.Add(warn2+lzch+assgn+lzch+IntToStr(sets[0, 2]));     {Voltage warning 2}
  liste.Add(OffKey+lzch+assgn+lzch+IntToStr(sets[0, 4]));    {Shutdown key}
  liste.Add(comment+' Shutdown/bind pin is input and needs a pull up resistor 10kOhm');
  liste.Add('');

  liste.Add(comment+' Correction factors');
  liste.Add(voltID+lzch+assgn+lzch+IntToStr(sets[0, 0]));    {Voltage}
  liste.Add(tasID+lzch+assgn+lzch+IntToStr(sets[0, 3]));     {PWM to TAS}
  liste.Add(aux5+lzch+assgn+lzch+IntToStr(sets[0, 5]));
  liste.Add('');
  liste.Add(comment+' Hardware PWM0:         0');
  liste.Add(comment+' Hardware PWM1:         1');
  liste.Add(comment+' GPIO number x (BCMx):  x');
  liste.Add(comment+' No GPIO port assigned: '+IntToStr(notused));
  liste.Add('');

  for i:=1 to 6 do begin                                {Write servos 1..6}
    liste.Add(ObjID1+svx+lzch+IntToStr(i)+ObjID2);      {Servo number}
    liste.Add(chnr+lzch+assgn+lzch+IntToStr(sets[i, 0]));
    liste.Add(svmin+lzch+assgn+lzch+IntToStr(sets[i, 1]));
    liste.Add(svntrl+lzch+assgn+lzch+IntToStr(sets[i, 2]));
    liste.Add(svmax+lzch+assgn+lzch+IntToStr(sets[i, 3]));
    liste.Add(pio+lzch+assgn+lzch+IntToStr(sets[i, 4]));
    if (sets[i, 4]>1) and (sets[i, 4]<notused) then begin   {Stick as 3-way switch}
      liste.Add(pio2+lzch+assgn+lzch+IntToStr(sets[i, 5]));
    end else begin
      rv:=false;                                        {Default: PWM not reverted}
      if sets[i, 5]>0 then
        rv:=true;                                       {PWM is reverted}
      liste.Add(pwmrev+lzch+assgn+lzch+BoolToStr(rv, true));
    end;
    liste.Add('');
  end;
  for i:=7 to 11 do begin                               {Write switches 1..5}
    liste.Add(ObjID1+swx+lzch+IntToStr(i-6)+ObjID2);
    liste.Add(chnr+lzch+assgn+lzch+IntToStr(sets[i, 0]));
    liste.Add(swup+lzch+assgn+lzch+IntToStr(sets[i, 1]));
    liste.Add(swmid+lzch+assgn+lzch+IntToStr(sets[i, 2]));
    liste.Add(swdown+lzch+assgn+lzch+IntToStr(sets[i, 3]));
    liste.Add(pio+lzch+assgn+lzch+IntToStr(sets[i, 4]));
    liste.Add(pio2+lzch+assgn+lzch+IntToStr(sets[i, 5]));
    liste.Add('');
  end;
  liste.Add(ObjID1+abtn+ObjID2);                        {Start/stop button}
  liste.Add(chnr+lzch+assgn+lzch+IntToStr(sets[12, 0]));
  liste.Add(actv+lzch+assgn+lzch+IntToStr(sets[12, 1]));
  liste.Add(pio+lzch+assgn+lzch+IntToStr(sets[12, 2]));
end;

{This shall be used if no settings file was found. It will be created from scratch
 using default settings.}

procedure WriteDefaultsSettings;                        {Array DefaultSetting into text file}
var
  liste: TStringList;

begin
  liste:=TStringList.Create;
  try
    liste.Add('# Default settings created at '+
              FormatDateTime('yyyy-mm-dd hh:nn:ss', now));
    liste.Add('');
    SettingsToText(DefaultSettings, liste);
    liste.SaveToFile(GetSettingsFile);
  finally
    liste.Free;
  end;
end;

{Read settings from text file (settings file) and fill settings array. Need to
 initialize the controls from settings.

 Settings array [index, column]:
 Index 0: Correction factors, voltage, warnings and Shutdownpin
          Shutdownpin must have pull-up resistor! PIO pin with shutdown button , 0 is active

 Index 1..6: Servos 1 to 6
 Index 7..11: Switches, 2-way or 3-way. For 3-way switches pio2 can be set to addess another GPIO port
 index 12: Start/stop button and some common values. Column 3 reserved for Servo PWM frequency

 Columns for switches: 0..channel, 1..up, 2..middle, 3..down, 4..GPIOnr, 5..GPIOnr2
 Columns for servos:   0..channel, 1..min, 2..neutral, 3..max, 4..GPIOnr, 5..Revers
 Column 12:            0..channel, 1..Red btn active, 2..GPIOnr, 3..PWMcycle, 4..n/a, 5..logging

 All default values in setting array remains untouched if something went wrong
 or is missing or there is no need to change it.}

function ReadSettingsList(inlist: TStringList): TSettings;
var
  s, objx: string;
  i, w: integer;
  idx: byte;

begin
  result:=DefaultSettings;                              {Fill with defaults}
  if inlist.Count>0 then begin
    for i:=0 to inlist.Count-1 do begin                 {Read the settings file line by line}
      s:=trim(inlist[i]);
      if (s<>'') and (s[1]<>comment) then begin         {Skip comments}
        if s[1]=ObjID1 then begin                       {Find sections}
          idx:=notused;
          objx:=trim(s.split([ObjID1, ObjID2])[1]);
          if objx.Split([lzch])[0]=svx then             {idx for Servos}
            idx:=StrToIntDef(objx.Split([lzch])[1], 1);
          if objx.Split([lzch])[0]=swx then             {idx for Switches}
            idx:=StrToIntDef(objx.Split([lzch])[1], 1)+6;
        end else begin                                  {Values with fix positions in array}
          if TryStrToInt(GetIDvalue(s, voltid), w) then begin
            result[0, 0]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, warn1), w) then begin
            result[0, 1]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, warn2), w) then begin
            result[0, 2]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, tasID), w) then begin
            result[0, 3]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, OffKey), w) then begin
            result[0, 4]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, aux5), w) then begin
            result[0, 5]:=w;
            Continue;
          end;

          if TryStrToInt(GetIDvalue(s, actv), w) then begin
            result[12, 1]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, pwmcycle), w) then begin
            result[12, 3]:=w;
            Continue;
          end;
          if TryStrToInt(GetIDvalue(s, logging), w) then begin
            result[12, 5]:=w;
            Continue;
          end;

          if idx<13 then begin                          {Values assigned via index}
            if TryStrToInt(GetIDvalue(s, chnr), w) then begin
              result[idx, 0]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, svmin), w) then begin
              result[idx, 1]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, svntrl), w) then begin
              result[idx, 2]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, svmax), w) then begin
              result[idx, 3]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, pio), w) then begin
              result[idx, 4]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, pwmrev), w) then begin
              result[idx, 5]:=w;
              Continue;
            end;

            if TryStrToInt(GetIDvalue(s, swup), w) then begin
              result[idx, 1]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, swmid), w) then begin
              result[idx, 2]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, swdown), w) then begin
              result[idx, 3]:=w;
              Continue;
            end;
            if TryStrToInt(GetIDvalue(s, pio2), w) then
              result[idx, 5]:=w;
          end;
        end;
      end;
    end;
  end;
end;

procedure ReadSettings(var sets: TSettings);            {Fill settings array from file}
var
  s: string;
  list: TStringList;

begin
  sets:=DefaultSettings;                                {Fill with defaults}
  s:=GetSettingsFile;
  if FileExists(s) then begin
    list:=TStringList.Create;
    list.LoadFromFile(s);
    try
      sets:=ReadSettingsList(list);
    finally
      list.Free;
    end;
  end;
end;

{Stick position from given channel will be converted to min/max, neutral values in settings.
 Input:  Stick values between 683 and 3412 (+/- 100%), neutral is 2048
 Output: integer value between min and max in settings}

function StkToPWM(sets: TSettings; Servo: byte;         {Analog stick position to PWM in ns}
                  value: uint16): uint64;
begin
  result:=value*1000;                                   {Output 1:1}
  if GetControlType(Servo)=1 then begin                 {Only for servos}
    if value>stkntrl then begin                         {Stick up}
      result:=sets[Servo, 2]*1000+
              round((value-stkntrl)*(sets[Servo, 3]-sets[Servo, 2])/
              (stkup-stkntrl)*1000);
    end else begin                                      {Stick down}
      result:=sets[Servo, 2]*1000-
              round((stkntrl-value)*(sets[Servo, 2]-sets[Servo, 1])/
              (stkntrl-stkdown)*1000);
    end;
  end;
end;


{Settings array [index, column]
 Columns for switches: 0..channel, 1..up, 2..middle, 3..down, 4..GPIOnr, 5..GPIOnr2
 Columns for servos:   0..channel, 1..min, 2..neutral, 3..max, 4..GPIOnr, 5..Revers
 Column 12:            0..channel, 1..Red btn active, 2..GPIOnr, 3..PWMcycle, 4..n/a, 5..logging}

function SwitchPos(sets: TSettings; switch: byte;       {Position up..1, middle..2, down..3}
                                    value: uint16; defaultpos: byte = 2): byte;
var
  i, t: byte;
  thr: uint16;

begin
  result:=defaultpos;
  t:=GetControlType(switch);
  if t=7 then begin                                     {Only for switches}
    for i:=1 to 3 do
      if value=sets[switch, i] then begin
        result:=i;
        exit
      end;
  end;

  if t=1 then begin                                     {Relax values from servos if sticks used as switches}
    result:=2;                                          {Default neutral, middle position}
    thr:=(sets[switch, 3]-sets[switch, 2]) div 3;
    if value>sets[switch, 2]+thr then
      result:=1                                         {Stick position like switch up}
    else
      if value<sets[switch, 2]-thr then
        result:=3;                                      {Stick position like switch down}
  end;
end;

{Get status of Start/stop button (the red one), active (true) means button pressed}

function StartStop(sets: TSettings; channel: byte;
                   value: uint16): boolean;             {Start/stop active or not}
begin
  result:=false;                                        {Red button not pressed}
  if (channel=sets[12, stkmin]) and (value=sets[12, 1]) then
    result:=true;                                       {Start/stop button active}
end;

{Stickposition to percent for Yuneec values (2048 +/- value}

function StkToProz(const w: uint16): int16;             {Stick Position to percent}
begin
  result:=round(w/stkmax*300)-150;
end;

{Convert an integer value from A/D converter to Yuneec telemetry format
 using to correction factor in settings array [0, 0]}

function VoltToTelemetry(sets: TSettings; volt: uint16): byte;  {Voltage following rule from Yuneec}
var
  v: integer;

begin
  result:=0;                                            {0 will be read as 5V}
  v:=round(volt*sets[0, 0]/100)-50;                     {Voltage in dV}
  if (v>0) and (v<256) then                             {Voltage between 5 and 25.5V}
    result:=v;
end;

end.
