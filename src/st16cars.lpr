{Control a model with ST10 or ST16 and ZigBee Receiver SR24.
 Configuration of HW made by config file. Two HW PWM channels for servos are
 supported and you can assign all switches to a GPIO output.

 st16cars needs to be in autostart:
 Add to crontab   @reboot /home/pi/st16cars

 }



program st16cars;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, crt, SR24_dec, SR24_ctrl, SR24_chsets;

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
  uartport='/dev/ttyAMA0';

{ st16car1 }

procedure GPIOon;                                    {Switch all used GPIO ports to out/0}
var
  i, g: byte;

begin
  for i:=1 to 11 do begin
    g:=csets[i, 4];
    if (g<notused) and (g>4) then
      ActivateGPIO(g);
    g:=csets[i, 5];
    if (g<notused) and (g>4) then                    {Will also activate GPIO ports for de-muxer}
      ActivateGPIO(g);
  end;
end;

procedure GPIOoff;                                   {Switch off all used GPIO ports}
var
  i, g: byte;

begin
  for i:=1 to 11 do begin
    g:=csets[i, 4];
    if (g<notused) and (g>4) then
      DeActivateGPIO(g);
    g:=csets[i, 5];
    if (g<notused) and (g>4) then
      DeActivateGPIO(g);
  end;
end;

procedure InitServos;                                {Initialize Servos. Only two can get HW PWM channels 0 or 1}
var
  i, pio: byte;

begin
  for i:= 1 to 6 do begin                            {For all 6 servos}
    pio:=csets[i, 4];
    if pio<2 then begin
      SetPWMChannel(pio, csets[12, 3],
                    csets[i, 2]*1000,                {Neutral position}
                    (csets[i, 5]=1));
    end;
  end;
end;

procedure ControlServos(dat: TPayLoad);              {Write pulse duration to PWM}
var
  i, pio: byte;

begin
  for i:=1 to 6 do begin                             {For all 6 servos}
    pio:=csets[i, 4];
    if pio<2 then
      SetPWMCycle(pio, StkToPWM(csets, i, GetChValue(dat, csets[i, 0])));
  end;
end;

procedure ControlSwitches(dat: TPayLoad);            {Send switches to GPIO port}
var
  i, pio, sw: byte;

begin
  for i:=1 to 11 do begin
    sw:=SwitchPos(csets, i, GetChValue(dat, csets[i, 0]));
    pio:=csets[i, 4];                                {First GPIO port for upper position as ON}
    if (pio<notused) and (pio>4) then begin
      if sw=1 then
        SetGPIO(pio, '1')
      else
        SetGPIO(pio, '0');
    end;

    pio:=csets[i, 5];                                {For 3-way switches use second GPIO, port}
    if (pio<notused) and (pio>4) then begin
      if sw=3 then                                   {Switch in lower position as ON}
        SetGPIO(pio, '1')
      else
        SetGPIO(pio, '0');
    end;
  end;
end;

procedure st16car1.DoRun;
var
  data, tele: TPayLoad;
  i, z, ExitStatus: byte;
  tlz, gps: uint16;
  coord: array [0..7] of byte;
  alt: single;

begin
  exitStatus:=0;                                     {Exit no faults}
  z:=0;
  tlz:=0;
  alt:=0;
  ActivatePWMChannel('2');                           {Activate both}
  if PWMstatus>1 then begin
    for i:=0 to 39 do                                {Load default values}
      tele[i]:=DefTelemetry[i];
    for i:=0 to high(data) do                        {Empty receive buffer}
      data[i]:=0;
    for i:=0 to high(coord) do                       {Empty coords buffer}
      coord[i]:=0;

    ReadSettings(csets);                             {Load common settings}
    InitServos;                                      {Set up PWM channels}
    GPIOon;
    ConnectUART(uartport, 115200, SR24connected);
    if SR24connected then begin

      if not UARTCanRead then begin                  {Wait for RC}
        tele[36]:=17;
        repeat
          sleep(timeout);
        until UARTCanRead or KeyPressed;
      end;

      if UARTCanRead then begin
        tele[36]:=16;
        repeat
          if UARTreadMsg(data) then begin
            ControlServos(data);                     {Servo assignement from settings}
            ControlSwitches(data);
            inc(z);

// This is just to show GPS data from the ST16 on the ST16
// where usually the data from the drone are seen (Mirroring).
// This could be removed or commented out
            if data[3]=3 then begin                  {Message type GPS data set}
              for i:=0 to 7 do
                coord[i]:=data[26+i];                {Store coordinates}
              alt:=GetFloatFromBuf(data, 34);        {Store altitude}
            end;
            if z>=5 then begin                       {One telemetry per 5 received packages}
              gps:=0;
              IntToTelemetry(data, tlz, 4, 2);       {Counter}
              for i:=0 to 7 do begin
                tele[i+6]:=coord[i];                 {Mirror coordinates}
                gps:=gps+Coord[i];                   {Check controller GPS if something >0 is there}
              end;
              IntToTelemetry(tele, AltitudeToInt(alt), 14, 4);  {Mirror Altitude m}
              i:=data[44];                           {nsat}
              tele[36]:=4;
              if gps>0 then begin
                i:=i or $80;                         {GPS of RC aquired}
                tele[36]:=3;
              end;
              tele[24]:=i;                           {nsat + GPS used}
              UARTsendMsg(tele);
              z:=0;                                  {Reset counter}
              inc(tlz);                              {Counter for sent packages}
              if tlz>=65535 then
                tlz:=0;
            end;
// End fake telemetry with coordinates
//          if z>10 then
//            UARTsendMsg(tele);      {Send default telemetry to avoid error messages on ST16}

          end else
            ExitStatus:=4;                           {no valid message}
        until KeyPressed;                            {stop program at any key}

      end else
        exitstatus:=3;                               {Cannot read}
    end else
      exitstatus:=2;                                 {not connected}
  end else
    exitstatus:=1;                                   {no PWM channels}
  DeactivatePWM;
  GPIOoff;
  DisconnectUART(SR24connected);
  writeln(ExitStatus);
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

