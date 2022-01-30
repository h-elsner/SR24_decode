{Logging

 Prepare a fake Logging like Yuneec legacy flight logs with
 data received from RC. Format can be reviewed by Q500log2kml.
 ---> see q500log2kml manual: http://h-elsner.mooo.com/pdf/Q500log2kml_en.pdf
                              https://github.com/h-elsner/Q500log2kml
}

unit SR24_log;

{$mode objfpc}{$H+}

interface

uses
  sysutils, fileutil;

const

  header_tele=',fsk_rssi,voltage,current,altitude,latitude,longitude,tas,gps_used,fix_type,satellites_num,roll,yaw,pitch,motor_status,imu_status,press_compass_status,f_mode,gps_status,vehicle_type,error_flags1,gps_accH';
  header_remote=',CH0,CH1,CH2,CH3,CH4,CH5,CH6,CH7,CH8,CH9,CH10,CH11,CH12,CH13,CH14,CH15,CH16,CH17,CH18,CH19,CH20,CH21,CH22,CH23';
  header_gps=',lon,lat,alt,accuracy,speed,angle';

  fldir='FlightLog_SR24';
  telemetry='Telemetry';
  remote='Remote';
  remotegps='RemoteGPS';
  csvext='.csv';

  sep=',';                                             {Data separator in log files}
  cff='0.000000';
  aff='0.00';
  ziffs=['0'..'9'];                                    {Valid digits in a string}

var
  MaxNoFiles: uint32=20;                               {Default max 20 logfiles, can be up to 99999}

function  ExtractNumber(s: string): string;            {Extract log number from file name}
function  GetFlightLogDirs(idx: byte): string;         {Get names of log root and sub directories}
function  MakeFlightLogDir: boolean;                   {Create root path for logfiles}
function  GetLogNumber: string;                        {Provides tail of next log file and create it}
function  GetTimeStamp: string;                        {Time stamp in Yuneec log format}

function  GetNumSat(n: byte): byte;                    {Get number of sats from RC}
function  GetFixType(n: byte): byte;                   {Get GPS fix type}
function  FixTypeToStr(const w: byte): string;         {MAVlink like GPS fix type to string}
function  GetGPSused(n: byte): boolean;                {Get GPS used flag}


implementation

function ExtractNumber(s: string): string;             {Extract log number from file name}
var
  i: integer;
  f: boolean;
  s1: string;

begin
  s1:='';
  f:=false;
  for i:=length(s) downto 1 do begin
    if s[i] in ziffs then begin
      s1:=s[i]+s1;
      f:=true;
    end else begin
      if f then
        break;
    end;
  end;
  result:=s1;
end;

function GetFlightLogDirs(idx: byte): string;          {Get names of log root (0) and sub directories (1..3)}
begin
  result:=IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0))+fldir);
  case idx of
    1: result:=IncludeTrailingPathDelimiter(result+telemetry);
    2: result:=IncludeTrailingPathDelimiter(result+remote);
    3: result:=IncludeTrailingPathDelimiter(result+remotegps);
  end;
end;

function MakeFlightLogDir: boolean;                    {Create root path for logfiles}
var
  path: string;

begin
  result:=false;                                       {Error during file access}
  path:=GetFlightLogDirs(0);
  if DirectoryExists(path) then begin
    result:=true;
    if not DirectoryExists(path+telemetry) then
      result:=result and CreateDir(path+telemetry);
    if not DirectoryExists(path+remote) then
      result:=result and CreateDir(path+remote);
    if not DirectoryExists(path+remotegps) then
      result:=result and CreateDir(path+remotegps);
  end else begin
    if CreateDir(path) then begin
      result:=CreateDir(path+telemetry) and
              CreateDir(path+remote) and
              CreateDir(Path+remotegps);
    end;
  end;
end;

function GetLogNumber: string;                         {Provides tail of next log file and create it}
var
  path: string;
  sr: TRawByteSearchRec;
  d, n: uint32;
  f: TextFile;

begin
  result:='';                                          {Error during file access}
  d:=0;
  path:=GetFlightLogDirs(1);
  if DirectoryExists(path) then begin
    if FindFirst(path+'*'+csvext, faAnyFile, sr)=0 then begin
      repeat                                           {Scan existing files}
        n:=StrToInt(ExtractNumber(sr.Name));
        if n>d then
          d:=n;
      until FindNext(sr)<>0;
      FindClose(sr);
    end;
    if d>=MaxNoFiles then begin
      if DeleteDirectory(GetFlightLogDirs(0), true) then
        MakeFlightLogDir;
      d:=0;
    end;
    result:='_'+Format('%0.5d', [d+1])+csvext;         {Tail for the 3 new log files}

    AssignFile(f, path+telemetry+result);              {Create files with header}
    try
      rewrite(f);
      writeln(f, header_tele);
    finally
      CloseFile(f);
    end;
    path:=GetFlightLogDirs(2);
    AssignFile(f, path+remote+result);
    try
      rewrite(f);
      writeln(f, header_remote);
    finally
      CloseFile(f);
    end;
    path:=GetFlightLogDirs(3);
    AssignFile(f, path+remotegps+result);
    try
      rewrite(f);
      writeln(f, header_gps);
    finally
      CloseFile(f);
    end;
  end;
end;

function GetTimeStamp: string;                         {Time stamp in Yuneec log format}
begin
  result:=FormatDateTime('YYYYMMDD hh:nn:ss:zzz', now);
end;

function GetNumSat(n: byte): byte;                     {Number of satellites}
begin
  result:=(n and $1F);
end;

function GetFixType(n: byte): byte;                    {FixType in nsat}
begin
  result:=(n shr 5) and 3;
end;

function FixTypeToStr(const w: byte): string;          {MAVlink like GPS fix type to string}
begin
  result:='';
  case GetFixType(w) of
    0:	Result:='No GPS connected';
    1:	Result:='No position information, GPS is connected';
    2:	Result:='2D position';
    3:	Result:='3D position';
  end;
end;

function GetGPSused(n: byte): boolean;                 {GPS used in nsat}
begin
  result:=(n and $80)<>0;
end;

end.
