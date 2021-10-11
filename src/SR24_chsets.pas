{Channel settings for ST10 or ST16

 Set relationship between channels and servo
 -------------------------------------------
 - assign channel to servo number (6 servos)
 - assign servo number to PWM or GPIO (6 servos)
 - set min, max and neutral per servo, deactivated gets neutral from pitch
 - set up, middle, down, GPIO port per switch (5 switches)
 - set push button (mixed to thr), default=0

Set unused servos and button to neutral, default values

}

unit SR24_chsets;

{$mode objfpc}{$H+}

interface

uses
  sysutils, classes, FileUtil;

type
  TSettings = array[0..12, 0..5] of integer;            {Channel settings array: servos, values}

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

  svx= 'Servo';
  swx= 'Switch';
  abtn='Start/stop';                                    {Start button mixed in channel x}

  voltid='Voltage';
  aux1=  'Aux 1';
  aux2=  'Aux 2';
  aux3=  'Aux 3';
  aux4=  'Aux 4';
  aux5=  'Aux 5';

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

  pwmcycle='PWM cycle';
  pwmrev=  'PWM reverse';

(*
  pwm0='PWM 0';                                         {Hardware PWM0 auf GPIO 18, pin 12}
  pwm1='PWM 1';                                         {Hardware PWM1 auf GPIO 13, pin 33}
  gpio5= 'GPIO 5';                                      {Pin 29}
  gpio6= 'GPIO 6';                                      {Pin 31}
  gpio12='GPIO 12';                                     {Pin 32}
  gpio16='GPIO 16';                                     {Pin 36}
  gpio17='GPIO 17';                                     {Pin 11}
  gpio22='GPIO 22';                                     {Pin 15}
  gpio23='GPIO 23';                                     {Pin 16}
  gpio24='GPIO 24';                                     {Pin 18}
  gpio25='GPIO 25';                                     {Pin 22}
  gpio26='GPIO 26';                                     {Pin 37}
  gpio27='GPIO 27';                                     {Pin 13; keep the other GPIO pins free for sensors}
*)

  comment='#';
  assgn=  ' = ';                                        {Separator between ID and value}
  ObjID1= '[';
  ObjID2= ']';
  lzch=   ' ';

  notused=88;

  DefaultSettings: TSettings =  {Correction factors for analog input: voltage, Aux1-Aux5}
                                ((1000, 1000, 1000, 1000, 1000, 1000),
                                {6 servos: channel, min, neutral, max, GPIOnr, PWM reverted (1)}
                                (1, 683, 2048, 3412, notused, 0), (2, 683, 2048, 3412, notused, 0),
                                (3, 683, 2048, 3412, notused, 0), (4, 683, 2048, 3412, notused, 0),
                                (7, 683, 2048, 3412, notused, 0), (8, 683, 2048, 3412, notused, 0),
                                {5 switches: channel, up, middle, down, GPIOnr, GPIOnr2}
                                (5, 3412, 2048, 683, notused, notused), (9, 2184, 2184, 3412, notused, notused),
                                (10, 683, 1502, 3412, notused, notused),(11, 4095, 4095, 0, notused, notused),
                                (12, 3412, 3412, 683, notused, notused),
                                {start/stop button: channel, active, GPIOnr and PWM cycle for all servos, n/a, n/a}
                                (1, 0, notused, 20000, 0, 0));

function GetSettingsFile(bak: boolean = false): string; {Get path and file name to settings}
function GetControlType(idx: byte): byte;               {Check if servos (1) or switches (7)}
procedure SettingsToText(const sets: TSettings; var liste: TStringlist);
procedure WriteDefaultsSettings;                        {Array DefaultSetting into text file}
procedure ReadSettings(var sets: TSettings);            {Fill settings array from file}

function StkToPWM(sets: TSettings; servo: byte;         {Analog stick position to PWM in ns}
                  value: uint16): uint64;
function SwitchPos(sets: TSettings; switch: byte;       {Position up..1, middle..2, down..3}
                   value: integer; defaultpos: byte = 2): byte;
function StartStop(sets: TSettings; channel: byte;
                   value: integer): boolean;            {Start/stop active or not}
function StkToProz(const w: uint16): int16;             {Stick Position to percent}
function VoltToTelemetry(sets: TSettings;               {Voltage to Yuneec format with correction factor}
                         volt: integer): byte;

implementation

{Find out what type of control it is depending on index.
 Output:  0..Correction factors,
          1..Servos 1 to 6
          7..Switches, 2-way or 3-way.
         12..Start/stop button and some common values
        >12..notused index, SW fault}

function GetControlType(idx: byte): byte;               {Check if servos (1) or switches (7)}
begin
  case idx of
    0: result:=0;                                       {Correction factors}
    1..6: result:=1;                                    {Servos}
    7..11: result:=7;                                   {Switches}
  else
    result:=idx;
  end;
end;

function GetSettingsFile(bak: boolean = false): string; {Get path and file name to settings file}
begin
  result:=ExtractFilePath(paramstr(0))+filename_settings;
  if bak then
    result:=ChangeFileExt(result, '.bak');
end;

{Check if enough lines in setting file (at least 2 lines) and
 if there are assignements parameter name to value in.
 Not a sharp valitity test but better than nothing.
 All default values in setting array remains untouched if something went wrong.
 Corrects the input if data separator is not ' = ', i.e. space forgotten}

function CheckSettings(var liste: TStringList): boolean;        {Correct data separator}
var
  i, p: integer;
  s: string;

begin
  result:=false;
  if liste.Count>1 then begin                                   {A minimum of settings}
    for i:=0 to liste.Count-1 do begin
      s:=liste[i];
      if (length(s)>5) and (s[1]<>comment) then begin
        p:=pos(assgn[2], s);
        if p>2 then begin

          result:=true;                                         {There possibly are settings in the list}
        end;
      end;
    end;
  end;
end;

{Read the settings array and write the related text file.
 This is used to create default settings file and for tests}

procedure SettingsToText(const sets: TSettings; var liste: TStringlist);  {Create text from settings}
var
  i: integer;
  rv: boolean;

begin
  liste.Add(comment+' Common settings');
  liste.Add(pwmcycle+assgn+IntToStr(sets[12, 3]));
  liste.Add('');

  liste.Add(comment+' Correction factor');
  liste.Add(voltid+assgn+IntToStr(sets[0, 0]));         {Voltage}
  liste.Add(aux1+assgn+IntToStr(sets[0, 1]));           {Aux 1 - Aux3}
  liste.Add(aux2+assgn+IntToStr(sets[0, 2]));
  liste.Add(aux3+assgn+IntToStr(sets[0, 3]));
  liste.Add(aux4+assgn+IntToStr(sets[0, 4]));
  liste.Add(aux5+assgn+IntToStr(sets[0, 5]));
  liste.Add('');
  liste.Add(comment+' Hardware PWM0:         0');
  liste.Add(comment+' Hardware PWM1:         1');
  liste.Add(comment+' GPIO number x (BCMx):  x');
  liste.Add(comment+' No GPIO port assigned: '+IntToStr(notused));
  liste.Add('');

  for i:=1 to 6 do begin                                {Write servos 1..6}
    liste.Add(ObjID1+svx+lzch+IntToStr(i)+ObjID2);      {Servo number}
    liste.Add(chnr+assgn+IntToStr(sets[i, 0]));
    liste.Add(svmin+assgn+IntToStr(sets[i, 1]));
    liste.Add(svntrl+assgn+IntToStr(sets[i, 2]));
    liste.Add(svmax+assgn+IntToStr(sets[i, 3]));
    liste.Add(pio+assgn+IntToStr(sets[i, 4]));
    rv:=false;                                          {PWM reverted}
    if sets[i, 5]=1 then
      rv:=true;                                         {PWM is revers}
    liste.Add(pwmrev+assgn+BoolToStr(rv, true));
    liste.Add('');
  end;
  for i:=7 to 11 do begin                               {Write switches 1..5}
    liste.Add(ObjID1+swx+lzch+IntToStr(i-6)+ObjID2);
    liste.Add(chnr+assgn+IntToStr(sets[i, 0]));
    liste.Add(swup+assgn+IntToStr(sets[i, 1]));
    liste.Add(swmid+assgn+IntToStr(sets[i, 2]));
    liste.Add(swdown+assgn+IntToStr(sets[i, 3]));
    liste.Add(pio+assgn+IntToStr(sets[i, 4]));
    liste.Add(pio2+assgn+IntToStr(sets[i, 5]));
    liste.Add('');
  end;
  liste.Add(ObjID1+abtn+ObjID2);                        {Start/stop button}
  liste.Add(chnr+assgn+IntToStr(sets[12, 0]));
  liste.Add(actv+assgn+IntToStr(sets[12, 1]));
  liste.Add(pio+assgn+IntToStr(sets[12, 2]));
end;

{This shall be used if no settings file was found. It will be created from scratch
 using default settings.}

procedure WriteDefaultsSettings;                        {Array DefaultSetting into text file}
var
  fn: string;
  liste: TStringList;

begin
  fn:=GetSettingsFile;
  if FileExists(fn) then
    CopyFile(fn, ChangeFileExt(fn, '.bak'));
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
 Index 0: Correction 5 factors, first column is reserved for voltage
 Index 1..6: Servos 1 to 6
 Index 7..11: Switches, 2-way or 3-way. For 3-way switches pio2 can be set to addess another GPIO port
 index 12: Start/stop button and some common values. Column 3 reserved for Servo PWM frequency

 Columns for switches: 0..channel, 1..up, 2..middle, 3..down, 4..GPIOnr, 5..GPIOnr2
 Columns for servos:   0..channel, 1..min, 2..neutral, 3..max, 4..GPIOnr, 5..Revers
 Index 12:             0..channel, 1..value for Active, 3..GPIOnr, 4..PWM period in micro sec, 5..n/a

 All default values in setting array remains untouched if something went wrong
 or is missing or there is no need to change it.}

procedure ReadSettings(var sets: TSettings);            {Fill settings array from file}
var
  s, objx: string;
  i, w: integer;
  idx: byte;
  inlist: TStringList;

begin
  sets:=DefaultSettings;                                {Fill with defaults}
  s:=GetSettingsFile;
  if FileExists(s) then begin
    inlist:=TStringList.Create;
    inlist.LoadFromFile(s);
    if CheckSettings(inlist) then begin
      try
        for i:=0 to inlist.Count-1 do begin             {Read the settings file line by line}
          s:=trim(inlist[i]);
          if (s<>'') and (s[1]<>comment) then begin     {Skip comments}
            if s[1]=ObjID1 then begin                   {Find sections}
              idx:=notused;
              objx:=trim(s.split([ObjID1, ObjID2])[1]);
              if objx.Split([lzch])[0]=svx then         {Servos}
                idx:=StrToIntDef(objx.Split([lzch])[1], 1);
              if objx.Split([lzch])[0]=swx then         {Switches}
                idx:=StrToIntDef(objx.Split([lzch])[1], 1)+6;
            end else begin                              {Values with fix positions in array}
              if s.Split([assgn])[0]=voltid then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 0]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=aux1 then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 1]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=aux2 then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 2]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=aux3 then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 3]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=aux4 then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 4]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=aux5 then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[0, 5]:=w;
                Continue;
              end;

              if s.Split([assgn])[0]=actv then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[12, 1]:=w;
                Continue;
              end;
              if s.Split([assgn])[0]=pwmcycle then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[12, 3]:=w;
                Continue;
              end;

              if idx<13 then begin                      {Values assigned via index}
                if s.Split([assgn])[0]=chnr then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 0]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=svmin then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 1]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=svntrl then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 2]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=svmax then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 3]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=pio then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 4]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=pwmrev then begin
                  if StrToBoolDef(trim(s.Split([assgn])[1]), false) then
                    sets[idx, 5]:=1;
                  Continue;
                end;

                if s.Split([assgn])[0]=swup then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 1]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=swmid then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 2]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=swdown then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 3]:=w;
                  Continue;
                end;
                if s.Split([assgn])[0]=pio2 then begin
                  if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                    sets[idx, 5]:=w;
                end;
              end;
            end;
          end;
        end;
      finally
        inlist.Free;
      end;
    end;
  end;
end;

{Stick position from given channel will be converted to min/max, neutral values in settings.
 Input:  Stick values between 683 and 3412 (+/- 100%), neutral is 2048
 Output: integer value between min and max in settings}

function StkToPWM(sets: TSettings; Servo: byte;       {Analog stick position to PWM in ns}
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
 Columns for servos: 0..channel, 1..min, 2..neutral, 3..max, 4..GPIOnr, 5..Revers}

function SwitchPos(sets: TSettings; switch: byte;      {Position up..1, middle..2, down..3}
                                    value: integer; defaultpos: byte = 2): byte;
var
  i, t: byte;

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
    if value>sets[switch, 2]+(sets[switch, 3] div 3) then
      result:=1                                         {Stick position like switch up}
    else
      if value<sets[switch, 2]-(sets[switch, 1] div 3) then
        result:=3;                                      {Stick position like switch down}
  end;
end;

{Get status of Start/stop button (the red one), active (true) means button pressed}

function StartStop(sets: TSettings; channel: byte;
                   value: integer): boolean;            {Start/stop active or not}
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

function VoltToTelemetry(sets: TSettings; volt: integer): byte;  {Voltage with correction factor}
var
  v: integer;

begin
  result:=0;
  v:=round(volt*sets[0, 0]/100)-50;                     {in dV}
  if (v>0) and (v<256) then                             {Voltage between 5 and 25.5V}
    result:=v;
end;

end.
