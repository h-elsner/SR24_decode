{Control a model with ST10 or ST16 and ZigBee Receiver SR24.
 Configuration of HW made by config file. Two HW PWM channels for servos are
 supported and you can assign all switches to a GPIO output.

 https://github.com/h-elsner/SR24_decode

 st16cars needs to be in autostart:
 Create a script and start the program as background process:
 ----------------------
 #!/bin/bash
 clear
 /home/pi/st16cars &
 -----------------------
 Save as rcautostart.sh,

 make it executable
 sudo chmod +x rcautostart.sh

 and add to /etc/rc.local before "exit 0":
 /home/pi/rcautostart.sh

 }

program st16cars;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, crt, SR24_dec, SR24_ctrl, SR24_chsets, mpu_ctrl, UNIX;

type

  { st16car1 }

  st16car1 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SR24Connected: boolean;
  csets: TSettings;

const
  ch='Ch';
  dgpio=': GPIO';
  tr='  ';

  { st16car1 }

procedure InitGPIO;                                  {Switch all used GPIO pins to out/0}
var
  i: byte;

begin
  for i:=1 to 11 do begin                            {For all servos and switches}
    if ValidGPIOnr(csets[i, 4]) then begin           {Activate GPIOnr pins}
      ActivateGPIO(csets[i, 4], 0);                  {As output}
      write(ch, csets[i, 0], dgpio, csets[i, 4], tr); {Info about settings in use}
    end;
    if ValidGPIOnr(csets[i, 5]) then begin           {Also activate GPIOnr2 pins}
      ActivateGPIO(csets[i, 5], 0);
      write(ch, csets[i, 0], dgpio, csets[i, 5], tr);
    end;
  end;
  writeln;
  if ValidGPIOnr(csets[0, 1]) then begin
    ActivateGPIO(csets[0, 1], 1);                    {As input}
    write('Warn1', dgpio, csets[i, 5], tr);
  end;
  if ValidGPIOnr(csets[0, 2]) then begin
    ActivateGPIO(csets[0, 2], 1);
    write('Warn2', dgpio, csets[i, 5]);
  end;
  writeln;
end;

procedure GPIOoff;                                   {Switch off all used GPIO ports}
var
  i: byte;

begin
  for i:=1 to 11 do begin                            {Deactivate output pins}
    if ValidGPIOnr(csets[i, 4]) then
      DeActivateGPIO(csets[i, 4]);
    if ValidGPIOnr(csets[i, 5]) then
      DeActivateGPIO(csets[i, 5]);
  end;

  if ValidGPIOnr(csets[0, 1]) then                   {Deactivate pins Voltage warnings}
    DeActivateGPIO(csets[0, 1]);
  if ValidGPIOnr(csets[0, 2]) then begin
    DeActivateGPIO(csets[0, 2]);
  end;
end;

procedure InitServos;                                {Initialize Servos. Only two can get HW PWM channels 0 or 1}
var
  i, pio: byte;

begin
  for i:= 1 to 6 do begin                            {For all 6 servos}
    pio:=csets[i, 4];
    if pio<2 then begin                              {PWM0 or 1}
      SetPWMChannel(pio, csets[12, 3],
                    csets[i, 2]*1000,                {Neutral position}
                    (csets[i, 5]=1));
      write(ch, csets[i, 0], ': PWM',pio, tr);       {Info about settings in use}
    end;
  end;
  writeln;
end;

procedure ControlServos(dat: TPayLoad);              {Write pulse duration to PWM}
var
  i: byte;

begin
  for i:=1 to 6 do                                   {For all 6 servos}
    if csets[i, 4]<2 then
      SetPWMCycle(csets[i, 4], StkToPWM(csets, i, GetChValue(dat, csets[i, 0])));
end;

procedure ControlSwitches(dat: TPayLoad);            {Send switches to GPIO port}
var
  i, sw: byte;
  onarr: array [2..27] of byte;

begin
  for i:=2 to 27 do                                  {Memory for all GPIO pins}
    onarr[i]:=notused;                               {All to invalid, need to be enabled below}

  for i:=1 to 11 do begin
    sw:=SwitchPos(csets, i, GetChValue(dat, csets[i, 0]));
    if ValidGPIOnr(csets[i, 4]) then begin           {First GPIO port for upper position as ON}
      if onarr[csets[i, 4]]<>1 then
        onarr[csets[i, 4]]:=0;                       {Reset if not set yet}
      if sw=1 then
        onarr[csets[i, 4]]:=1;
    end;

    if ValidGPIOnr(csets[i, 5]) then begin           {For 3-way switches use second GPIO, port}
      if onarr[csets[i, 5]]<>1 then
        onarr[csets[i, 5]]:=0;                       {Reset if not set yet}
      if sw=3 then
        onarr[csets[i, 5]]:=1;
    end;
  end;

////////////////////////////////////////////////////////////////////////////////
{Special, HW dependend functionality: Hazard lights on Tilt mode to GPIO 16 and 17}
  if SwitchPos(csets, 8, GetChValue(dat, csets[8, 0]))=3 then begin
    onarr[16]:=1;
    onarr[17]:=1;
  end;
////////////////////////////////////////////////////////////////////////////////

  for i:=2 to 27 do begin                            {Execute value to GPIO pin}
    if onarr[i]=1 then
      SetGPIO(i, GPIOhigh);
    if onarr[i]=0 then
      SetGPIO(i, GPIOlow);
  end;
end;


function IsStop: boolean;
begin
  result:=false;
  if keypressed then
    if readkey='x' then                              {Manual stop of the loop with 'x'-key}
      result:=true;
end;

procedure st16car1.DoRun;
var
  ErrorMsg: string;
  data, tele: TPayLoad;
  i16: int16;                                        {2byte values for telemetry}
  i, z: byte;
  gps: uint16;
  coord: array [0..7] of byte;
  alt: single;
  gohalt, bindmode, offenabled, mpu: boolean;

begin
  ErrorMsg:='No IMU';                                {Exit no faults, no IMU available}
  SR24connected:=false;
  z:=0;
  alt:=0;
  gohalt:=false;
  bindmode:=false;
  offenabled:=true;
  if ActivatePWMChannel(true)>1 then begin           {At least one channel activ}
    for i:=0 to 39 do                                {Load default values for telemetry}
      tele[i]:=DefTelemetry[i];
    for i:=0 to high(data) do                        {Empty receive buffer}
      data[i]:=0;
    for i:=0 to high(coord) do                       {Empty coords buffer}
      coord[i]:=0;

    mpu:=GetAdrStrMPU;                               {Check if MPU6050 is available}
    if mpu then begin
      ErrorMsg:='OK';                                {Exit no faults}
      MPUWakeUp;
      SetReg(MPUadr, 27, 0);                         {afs_sel=0 - +/-2G}
      tele[34]:=tele[34] or 1;                       {IMU bit set in IMU_status}
    end;

    ReadSettings(csets);                             {Load common settings from text file}
    InitServos;                                      {Set up PWM channels}
    InitGPIO;
    if ValidGPIOnr(csets[0, 4]) then                 {Shutdown key}
      ActivateGPIO(csets[0, 4], 1);                  {GPIO pin must have pull-up resistor!}

    ConnectUART(uartport, UARTspeed, SR24connected);
    if SR24connected then begin

      if not UARTCanRead then begin                  {Wait for RC}
        tele[36]:=17;                                {Flight mode}
        repeat
          if (not bindmode) and ShutDownButton(csets[0, 4]) then begin
            bindmode:=true;                          {Send Bind messages only once}
            offenabled:=false;                       {Stop shutdown function for some time}
            SendBind;
            z:=0;
////////////////////////////////////////////////////////////////////////////////
{special, HW dependend functionality: Front light to indicate bind mode}
            SetGPIO(26, GPIOhigh);
////////////////////////////////////////////////////////////////////////////////
          end;

          sleep(timeout);
          inc(z);
          if z>20 then begin                         {Wait some time depending on timeout; default 3s}
            offenabled:=true;                        {Key becomes shutdown key again}
            z:=0;
          end;
          gohalt:=offenabled and ShutDownButton(csets[0, 4]);
        until UARTCanRead or IsStop or gohalt;
      end;

      z:=0;
      if UARTCanRead then begin                      {ST16 connected}
        tele[36]:=16;                                {Flight mode init}
        bindmode:=false;

        repeat                                       {Message loop}
          if UARTreadMsg(data) then begin
            ControlServos(data);                     {Servo assignement from settings}
            ControlSwitches(data);
            inc(z);                                  {Telemetry counter}

            if data[3]=3 then begin                  {Message type GPS data set}
              for i:=0 to 7 do
                coord[i]:=data[26+i];                {Store coordinates}
              alt:=GetFloatFromBuf(data, 34);        {Store altitude}
            end;

            if z>=5 then begin                       {One telemetry msg per 5 received msgs}
              i:=csets[0, 1];
              if ValidGPIOnr(i) and (GetGPIO(i)=GPIOhigh) then
                tele[38]:=(tele[38] or 1) and $FD;   {Voltage warning 1}
              i:=csets[0, 2];
              if ValidGPIOnr(i) and (GetGPIO(i)=GPIOhigh) then
                tele[38]:=(tele[38] or 2) and $FE;   {Voltage warning 2}

              gps:=0;
              for i:=0 to 7 do begin
                tele[i+6]:=coord[i];                 {Mirror coordinates}
                gps:=gps+Coord[i];                   {Check controller GPS if something <>0 is there}
              end;
              IntToTelemetry(tele, AltitudeToInt(alt), 14, 4);  {Mirror Altitude m}
              i:=data[44];                           {nsat}
              tele[36]:=4;                           {Angle w/o GPS, but it means here w/o GPS from RC}
              if gps<>0 then begin
                i:=i or $80;                         {GPS of RC aquired}
                tele[36]:=3;                         {Flight mode}
                tele[34]:=tele[34] or $20;           {GPS2 in IMU status available}
              end;
              tele[24]:=i;                           {nsat + GPS used}

              if mpu then begin                      {IMU MPU6050 available}
                i16:=GetRegWbe(MPUadr, 61);          {Accel_Y}
                i16:=round(i16/16384*csets[0, 5]);   {Correction factor AUX5}
                tele[27]:=i16 and $ff;               {Roll_L}
                tele[28]:=(i16 shr 8) and $FF;       {Roll_H}

                i16:=GetRegWbe(MPUadr, 63);          {Accel_Z}
                i16:=-round((i16/16384-1)*csets[0, 5]);
                tele[29]:=i16 and $ff;               {Pitch_L}
                tele[30]:=(i16 shr 8) and $FF;       {Pitch_H}

                i16:=GetRegWbe(MPUadr, 59);          {Accel_X}
                i16:=round((i16/16384-1)*csets[0, 5]);
                tele[31]:=i16 and $ff;               {Yaw_L}
                tele[32]:=(i16 shr 8) and $FF;       {Yaw_H}
              end;

              UARTsendMsg(tele);
              z:=0;                                  {Reset counter}
            end;
          end else
            ErrorMsg:='No valid messages';           {No valid message found}
          gohalt:=ShutdownButton(csets[0, 4]);
        until IsStop or gohalt;                      {Stop program with 'x' key or HW-button}
      end else
        ErrorMsg:='Cannot read from UART';           {Cannot read from UART}

    end else
      ErrorMsg:='Not connected';                     {UART not connected}
  end else
    ErrorMsg:='No PWM channels';                     {No PWM channels available}

////////////////////////////////////////////////////////////////////////////////
{special, HW dependend functionality: Retract camera at stop}
  SetGPIO(24, GPIOlow);
  SetGPIO(23, GPIOhigh);
  sleep(1000);                                       {Wait until camera came down}
////////////////////////////////////////////////////////////////////////////////

  DeactivatePWM;                                     {Clean up all the stuff}
  GPIOoff;
  DeactivateGPIO(csets[0, 4]);                       {Shutdown pin}
  DisconnectUART(SR24connected);
  writeln(ErrorMsg);
  if gohalt then                                     {Shutdown only with HW-Button}
    fpSystem('sudo halt');
  Terminate;
end;

constructor st16car1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor st16car1.Destroy;
begin
  inherited Destroy;
end;

var
  Application: st16car1;
begin
  Application:=st16car1.Create(nil);
  Application.Title:='ST16 car control';
  Application.Run;
  Application.Free;
end.

