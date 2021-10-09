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

uses
  sysutils, classes, FileUtil;

type
  TSettings = array[0..12, 0..3] of integer;    {Channel settings array: servos, values}

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

  chnr=  'Channel';
  svmin= 'Minimum';
  svmax= 'Maximum';
  svntrl='Neutral';

  swup=  'Upper  position';
  swmid= 'Middle position';
  swdown='Lower  position';
  actv=  'Active';

  pwmcycle='PWM cycle';
  pwmrev=  'PWM reverse';

  comment='#';
  assgn=  ' = ';                                        {Separator between ID and value}
  ObjID1= '[';
  ObjID2= ']';
  lzch=   ' ';

  DefaultSettings: TSettings =  {Correction factors for analog input: voltage, Aux1-Aux3}
                                ((1000, 1000, 1000, 1000),
                                {6 servos: channel, min., neutral, max}
                                (1, 683, 2048, 3412), (2, 683, 2048, 3412),
                                (3, 683, 2048, 3412), (4, 683, 2048, 3412),
                                (7, 683, 2048, 3412), (8, 683, 2048, 3412),
                                {5 switches: channel, up, middle, down}
                                (5, 3412, 2048, 683), (9, 2184, 2184, 3412),
                                (10, 683, 1502, 3412),(11, 4095, 4095, 0),
                                (12, 3412, 3412, 683),
                                {start/stop button: channel, active and PWM cycle, PWM reverted (1)}
                                (1, 0, 20000, 0));

function GetSettingsFile(bak: boolean = false): string; {Get path and file name to settings}
procedure SettingsToText(const sets: TSettings; var liste: TStringlist);
procedure WriteDefaultsSettings;                        {Array DefaultSetting into text file}
procedure ReadSettings(var sets: TSettings);            {Fill settings array from file}

function StkToPWM(sets: TSettings; channel: byte;       {Analog stick position to PWM in ns}
                  value: integer): uint32;
function SwitchPos(sets: TSettings; channel: byte;      {Position up..1, middle..2, down..3}
                   value: integer; defaultpos: byte = 1): byte;
function StartStop(sets: TSettings; channel: byte;
                   value: integer): boolean;            {Start/stop active or not}
function StkToProz(const w: uint16): int16;             {Stick Position to percent}
function VoltToTelemetry(sets: TSettings; volt: integer): byte;  {Voltage with correction factor}

implementation

function FindChannel(sets: TSettings; ch: byte): byte;  {Find index in array for given chnannel}
var
  i: integer;

begin
  result:=88;                                           {Invalid channel number}
  for i:=1 to 12 do
    if ch=sets[i , 0] then begin
      result:=i;
      exit;
    end;
end;

function GetSettingsFile(bak: boolean = false): string; {Get path and file name to settings}
begin
  result:=ExtractFilePath(paramstr(0))+filename_settings;
  if bak then
    result:=ChangeFileExt(result, '.bak');
end;

procedure SettingsToText(const sets: TSettings; var liste: TStringlist);  {Create text from sttings array}
var
  i: integer;
  rv: boolean;

begin
  liste.Add('# '+svx+' PWM settings');
  liste.Add(pwmcycle+assgn+IntToStr(sets[12, 2]));
  rv:=false;                                            {PWM reverted}
  if sets[12, 3]=1 then
    rv:=true;                                           {PWM is revers}
  liste.Add(pwmrev+assgn+BoolToStr(rv, true));
  liste.Add('');

  liste.Add('# Correction factor');
  liste.Add(voltid+assgn+IntToStr(sets[0, 0]));         {Voltage}
  liste.Add(aux1+assgn+IntToStr(sets[0, 1]));           {Aux 1 - Aux3}
  liste.Add(aux2+assgn+IntToStr(sets[0, 2]));
  liste.Add(aux3+assgn+IntToStr(sets[0, 3]));
  liste.Add('');

  for i:=1 to 6 do begin                                {Write servos 1..6}
    liste.Add(ObjID1+svx+lzch+IntToStr(i)+ObjID2);      {Servo number}
    liste.Add(chnr+assgn+IntToStr(sets[i, 0]));
    liste.Add(svmin+assgn+IntToStr(sets[i, 1]));
    liste.Add(svntrl+assgn+IntToStr(sets[i, 2]));
    liste.Add(svmax+assgn+IntToStr(sets[i, 3]));
    liste.Add('');
  end;
  for i:=7 to 11 do begin                               {Write switches 1..5}
    liste.Add(ObjID1+swx+lzch+IntToStr(i-6)+ObjID2);
    liste.Add(chnr+assgn+IntToStr(sets[i, 0]));
    liste.Add(swup+assgn+IntToStr(sets[i, 1]));
    liste.Add(swmid+assgn+IntToStr(sets[i, 2]));
    liste.Add(swdown+assgn+IntToStr(sets[i, 3]));
    liste.Add('');
  end;
  liste.Add(ObjID1+abtn+ObjID2);                        {Start/stop button}
  liste.Add(chnr+assgn+IntToStr(sets[12, 0]));
  liste.Add(actv+assgn+IntToStr(sets[12, 1]));
end;

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
    try
      for i:=0 to inlist.Count-1 do begin               {Read the settings file line by line}
        s:=trim(inlist[i]);
        if (s<>'') and (s[1]<>comment) then begin       {Skip comments}
          if s[1]=ObjID1 then begin                     {Find sections}
            idx:=88;
            objx:=trim(s.split([ObjID1, ObjID2])[1]);
            if objx.Split([lzch])[0]=svx then           {Servos}
              idx:=StrToIntDef(objx.Split([lzch])[1], 1);
            if objx.Split([lzch])[0]=swx then
              idx:=StrToIntDef(objx.Split([lzch])[1], 1)+6;
            if objx.Split([lzch])[0]=abtn then
              idx:=12;
          end else begin                                {Values with fix positions in array}
            if s.Split([assgn])[0]=voltid then begin
              if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                sets[0, 0]:=w;
            end;
            if s.Split([assgn])[0]=aux1 then begin
              if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                sets[0, 1]:=w;
            end;
            if s.Split([assgn])[0]=aux2 then begin
              if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                sets[0, 2]:=w;
            end;
            if s.Split([assgn])[0]=aux3 then begin
              if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                sets[0, 3]:=w;
            end;

            if s.Split([assgn])[0]=pwmcycle then begin
              if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                sets[12, 2]:=w;
            end;
            if s.Split([assgn])[0]=pwmrev then begin
              if StrToBoolDef(trim(s.Split([assgn])[1]), false) then
                sets[12, 3]:=1;
            end;

            if idx<13 then begin                        {Values assigned via index}
              if s.Split([assgn])[0]=chnr then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 0]:=w;
              end;
              if s.Split([assgn])[0]=svmin then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 1]:=w;
              end;
              if s.Split([assgn])[0]=svntrl then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 2]:=w;
              end;
              if s.Split([assgn])[0]=svmax then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 3]:=w;
              end;
              if s.Split([assgn])[0]=swup then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 1]:=w;
              end;
              if s.Split([assgn])[0]=swmid then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 2]:=w;
              end;
              if s.Split([assgn])[0]=swdown then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 3]:=w;
              end;
              if s.Split([assgn])[0]=actv then begin
                if TryStrToInt(trim(s.Split([assgn])[1]), w) then
                  sets[idx, 1]:=w;
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

function StkToPWM(sets: TSettings; channel: byte;       {Analog stick position to PWM in ns}
                  value: integer): uint32;
var
  idx: byte;

begin
  result:=value*1000;                                   {Output 1:1}
  idx:=FindChannel(sets, channel);
  if idx<13 then begin
    if value=stkmin then                                {Ignore start/stop button}
      value:=stkdown;
    if value>stkntrl then begin                         {Stick up}
      result:=sets[idx, 2]*1000+
              round((value-stkntrl)*(sets[idx, 3]-sets[idx, 2])/
              (stkup-stkntrl)*1000);
    end else begin                                      {Stick down}
      result:=sets[idx, 2]*1000-
              round((stkntrl-value)*(sets[idx, 2]-sets[idx, 1])/
              (stkntrl-stkdown)*1000);
    end;
  end;
end;

function SwitchPos(sets: TSettings; channel: byte;      {Position up..1, middle..2, down..3}
                   value: integer; defaultpos: byte = 1): byte;
var
  idx, i: byte;

begin
  result:=defaultpos;
  idx:=FindChannel(sets, channel);
  if idx<13 then begin
    for i:=1 to 3 do
      if value=sets[idx,i] then begin
        result:=i;
        exit
      end;
  end;
end;

function StartStop(sets: TSettings; channel: byte;
                   value: integer): boolean;            {Start/stop active or not}
begin
  result:=false;
  if (channel=sets[12, stkmin]) and (value=sets[12, 1]) then
    result:=true;                                       {Start/stop button active}
end;

function StkToProz(const w: uint16): int16;             {Stick Position to percent}
begin
  result:=round(w/stkmax*300)-150;
end;

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
