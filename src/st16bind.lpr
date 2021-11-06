program st16bind;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, SR24_dec;

type
  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ Send bind message 5 times }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  SR24Connected: boolean;
  i: byte;

const
  uartport='/dev/ttyAMA0';

begin
  ErrorMsg:='OK done';
  SR24connected:=false;
  ConnectUART(uartport, UARTspeed, SR24connected);
  if SR24connected then begin
    if UARTCanWrite then begin
      for i:=0 to 4 do begin
        SendBind;
        sleep(200);
      end;
    end else
      ErrorMsg:='Cannot write to UART';
  end else
    ErrorMsg:='Not connected';
  DisconnectUART(SR24connected);
  writeln(ErrorMsg);
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

