{From ST24.h and ST24.c PX4 Autopilot
 Read data from SR24

 Uses synaser from Synapse (install per Online package manager).
 Then open package file laz_synapse.lpk and Add to project.
 Datenstruktur

byte idx val   desrcription
 0       $55   header 1
 1       $55   header 2
 2       len   24/43 length data after len byte inclusive type and CRC8, max 64
 3       0..3  Msg type: CHANNELDATA12      = 0                len $18  24
	                 CHANNELDATA24      = 1
                         Telemetry to RC    = 2                len $26  38
	                 TRANSMITTERGPSDATA = 3                len $2B  43
 4       Counter
 5       ??    Random?
 6       RSSI  (in % ?)
 7       Package counter
 8       from here Payload (Channels / GPS data) ...
...
 len+2   $xx   CRC8

 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 num bytes
 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 idx bytes
         |---------------------------------------------------------------------|   No bytes in Ln   Ln=24 for type 0 / Ln=43 for type 3 (GPS data)
      |---------------------------------------------------------------------|      bytes for CRC8
h1 h2 Ln Tp Cntr  Ri Pc Ch0 Ch1  Ch2 Ch3  Ch4 Ch5  Ch6 Ch7  Ch8 Ch9  Ch10Ch11 CRC8                                                     CRC8
}
