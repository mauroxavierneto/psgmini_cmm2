' PSGMINI - VGM PLAYER WITH
' SN76489 / NES APU SIMULATOR
'
' MAURO XAVIER 2020
'
' VERSION 0.3A

OPTION CONSOLE SCREEN

'INIT SOUND AND VARIABLES
Sub InitSound 
 If StartedPSG=0 then
  Dim ChVol(5),Ton(5),MUS%(40000),ChType$(5),ChgChan(5),ChSide$(5),MaxVol(5): StartedPSG=1
  Dim TonA$(5) LENGTH 16,TonB$(5) LENGTH 16
  Dim TonAnt(5),ChVolAnt(5)
  Dim Counter(5),ConstVol(5),HaltFlag(5),StartFlag(5),EnableCh(5),BkpVol(5)
  Dim NESLTab(32) LENGTH 5

  ' ** NES APU LENGTH TABLE **
  ' LINEAR LENGTH VALUES
  NESLTab(&H1F)=30
  NESLTab(&H1D)=28
  NESLTab(&H1B)=26
  NESLTab(&H19)=24
  NESLTab(&H17)=22
  NESLTab(&H15)=20
  NESLTab(&H13)=18
  NESLTab(&H11)=16
  NESLTab(&H0F)=14
  NESLTab(&H0D)=12
  NESLTab(&H0B)=10
  NESLTab(&H09)=8
  NESLTab(&H07)=6
  NESLTab(&H05)=4
  NESLTab(&H03)=2
  NESLTab(&H01)=254

  'NOTES WITH BASE LENGTH 12 (4/4 AT 75 BPM)
  NESLTab(&H1E)=32
  NESLTab(&H1C)=16
  NESLTab(&H1A)=72
  NESLTab(&H18)=192
  NESLTab(&H16)=96
  NESLTab(&H14)=48
  NESLTab(&H12)=24
  NESLTab(&H10)=12

  'NOTES WITH BASE LENGTH 10 (4/4 AT 90 BPM)
  NESLTab(&H0E)=26
  NESLTab(&H0C)=14
  NESLTab(&H0A)=60
  NESLTab(&H08)=160
  NESLTab(&H06)=80
  NESLTab(&H04)=40
  NESLTab(&H02)=20
  NESLTab(&H00)=10
 endif
 speed=1
 NESCLK=1789733
 LONGSTRING CLEAR MUS%()
 Play Stop
End Sub

'CHANGE MUSIC SPEED (USE IT AFTER PLAY)
Sub ChangeSpeed(xx)
 Local SRate
 SRate=44100
 If xx=0 Then
  W60=0:W50=0:SWait=0
 Else
  W60=(1000/(SRate/735))/xx:W50=(1000/(SRate/882))/xx
  SWait=(1000/SRate)/xx
 EndIf
 Speed=xx
End Sub

' SEEK INTEGER NUMBER
Function SkInt(HMus,size)
 Local Temp$
 Temp$="": For SMus=size To 1 Step -1
  Seek #4,HMus+SMus:Temp$=Temp$+Hex$(Asc(Input$(1,#4)))
 Next SMus:SkInt=Val("&H"+Temp$)
End Function

' LOAD VGM FILE AND RESET CHANNELS
Sub LoadVGM(Music$)
 Local T,MusBuf
 Play Stop
 Open Music$ For INPUT As #4
 FOR T=0 TO 5
  SetChannelType(T+1,"Q"): SetChannelVol(T+1,15)
  ChgChan(T)=0: ChVol(T)=15: BkpVol(T)=15: ChSide$(T)="B": Ton(T)=1
  TonA$(T)="00000000": TonB$(T)="00000000": TonAnt(T)=0:ChVolAnt(T)=15
  Counter(T)=0: ConstVol(T)=0: HaltFlag(T)=0:FrameMode=0:FrameInt=0: SubSweep=120:SubEnv=240
  StartFlag(T)=0:EnableCh(T)=1
 NEXT T
 SetChannelType(4,"N")
 Speed=1:VGMoffset=SkInt(&H04,4):VGMO=SkInt(&H34,4)
 If VGMO=0 Then VGMO=&H40
 LOff=SkInt(&H1C,4)-8:GoVGM:Rtim=1
 If Loff<&H40 then Loff=VGMO-2 
 Seek #4,VGMO-2
 LMus=Lof(#4)
 PTim=0:STim=0:MP=0:MusBuf=250:T=0
 DO WHILE NOT(EOF(#4))
  T=T+250
  IF T>LMus then MusBuf=250-(T-LMus)
  LONGSTRING APPEND MUS%(),INPUT$(MusBuf,#4)
 LOOP 
 Close #4
 Play Stop
End Sub


' VERIFY LOOP, OFFSET AND VGM RESTART
Sub GoVGM
  if MP>LMus Then StopVGM:GoTo NoSound
  If MP>=LMus And Loff>0 Then MP=Loff
  NoSound:
End Sub

' PLAY VGM FILE
Sub PlayVGM 
 ChangeSpeed(Speed)
 MP=VGMO-2:If MP>=LMus And Loff>0 Then MP=Loff
 PTim=0:STim=0:MP=0
 SetTick 10,IntSND,4
End Sub

' SET INTERRUPT (USE IT AFTER PLAY COMMAND)
Sub SetIntVGM(tickMus)
 SetTick tickMus,IntSND,4
End Sub

' STOP VGM BUT NOT UNLOAD
Sub StopVGM
 SetIntVGM(0):Pause(100):Play Stop
End Sub

' STOP VGM AND UNLOAD
Sub UnloadVGM
 SetIntVGM(0):StopVGM:Pause(100):Play Stop:LONGSTRING CLEAR MUS%():Ra$=""
End Sub

' SET CHANNEL TYPE
Sub SetChannelType(Ch,Type$)
 ChType$(Ch-1)=Type$
End Sub

' SET CHANNEL SIDE
Sub SetChannelSide(Ch,Side$)
 ChSide$(Ch-1)=Side$
End Sub

' SET MAXIMUM CHANNEL VOLUME
Sub SetChannelVol(Ch,Vl)
 MaxVol(Ch-1)=(15-Vl)
End Sub

' VGM INTERRUPTION ROUTINE - SN76489 SIMULATOR
Sub IntSND
 Local H,M$,T,TT,SN, Frames
 GoVGM
 Do While (Timer-STim<=PTim):Rtim=1:Exit Sub:Loop
 If Rtim=1 Then STim=Timer:PTim=SWait:Rtim=0
 MP=MP+1:H=Peek(VAR MUS%(),MP)
 SELECT CASE H

 ' *** TEXAS INSTRUMENTS SN76489 SIMULATION ***
 
 CASE &H50:
  ChgChan(0)=0:ChgChan(1)=0:ChgChan(2)=0:ChgChan(3)=0
  MP=MP+1:M$=Bin$(Peek(VAR MUS%(),MP))
  If Left$(M$,1)="1" Then ' LATCH
   Ch=Val("&B"+Mid$(M$,2,2))
   If Mid$(M$,4,1)="0" Then ' SOUND    
    if Ch=3 then
     IF Mid$(M$,6,1)="1" THEN ' WHITE NOISE
      Ton(3)=10200-Val("&B"+Right$(M$,2))*1500: ChType$(3)="N"
      Ton(3)=(3579545/(Ton(Ch)*48))
     ELSE ' "PERIODIC" NOISE      
      SELECT CASE Val("&B"+Right$(M$,2))
       CASE 0: Ton(3)=40: ChType$(3)="W2"
       CASE 1: Ton(3)=30: ChType$(3)="W2"
       CASE 2: Ton(3)=1:  ChType$(3)="P"
       CASE 3: Ton(3)=110: ChType$(3)="W2"
      END SELECT
     ENDIF
    else ' TONE
     Ton(Ch)=Val("&B"+Right$(Bin$(Peek(VAR MUS%(),MP+2)),6)+Right$(M$,4))+1
     Ton(Ch)=(3579545/(Ton(Ch)*32))
    endif
    MP=MP+2
    If Ton(ch)>20000 Then Ton(ch)=1
    ChgChan(Ch)=1
   Else ' VOLUME
    ChgChan(Ch)=1
    ChVol(Ch)=Val("&B"+Right$(M$,4))
    ChVol(Ch)=ChVol(Ch)+MaxVol(Ch): if ChVol(Ch)>15 then ChVol(Ch)=15
   EndIf   
   if ChVol(Ch)<15 and Ton(Ch)>0 and ChgChan(Ch)=1 then 
    SN=Ton(Ch)
    SELECT CASE ChSide$(Ch)
     CASE "B":
      SELECT CASE ChType$(Ch)
       CASE "N":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,B,N,SN,T
       CASE "P":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,B,P,SN,T
       CASE "Q":  Play Sound Ch+1,B,Q,SN,15-ChVol(Ch)
       CASE "W":  Play Sound Ch+1,B,W,SN,25-ChVol(Ch)*1.5
       CASE "W2": Play Sound Ch+1,B,W,SN,15-ChVol(Ch)
       CASE "S":  Play Sound Ch+1,B,S,SN,25-ChVol(Ch)*1.5
       CASE "T":  Play Sound Ch+1,B,T,SN,25-ChVol(Ch)*1.5
      END SELECT
     CASE "L":
      SELECT CASE ChType$(Ch)
       CASE "N":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,L,N,SN,T
       CASE "P":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,L,P,SN,T
       CASE "Q":  Play Sound Ch+1,L,Q,SN,15-ChVol(Ch)
       CASE "W":  Play Sound Ch+1,L,W,SN,25-ChVol(Ch)*1.5
       CASE "W2": Play Sound Ch+1,L,W,SN,15-ChVol(Ch)
       CASE "S":  Play Sound Ch+1,L,S,SN,25-ChVol(Ch)*1.5
       CASE "T":  Play Sound Ch+1,L,T,SN,25-ChVol(Ch)*1.5
      END SELECT
     CASE "R":
      SELECT CASE ChType$(Ch)
       CASE "N":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,R,N,SN,T
       CASE "P":  
        T=25-ChVol(Ch)*1.7: IF T<1 THEN T=1
        Play Sound Ch+1,R,P,SN,T
       CASE "Q":  Play Sound Ch+1,R,Q,SN,15-ChVol(Ch)
       CASE "W":  Play Sound Ch+1,R,W,SN,25-ChVol(Ch)*1.5
       CASE "W2": Play Sound Ch+1,R,W,SN,15-ChVol(Ch)
       CASE "S":  Play Sound Ch+1,R,S,SN,25-ChVol(Ch)*1.5
       CASE "T":  Play Sound Ch+1,R,T,SN,25-ChVol(Ch)*1.5
      END SELECT
    END SELECT
   else
    if ChType$(Ch)<>"" then Play Sound Ch+1,B,O,1,1
   endif
  endif 

 ' *** NINTENDO NES APU SIMULATION ***

 CASE &HB4:
  MP=MP+1:MHEX=Peek(VAR MUS%(),MP)
  SELECT CASE MHEX
   CASE &H00 TO &H1F, &H40 TO &H7F: NESADR=(MHEX+&H4000)
   CASE &H20 TO &H3E: NESADR=(MHEX+&H4060)
   CASE &H3F: NESADR=&H4023   
  END SELECT
  MP=MP+1:M$=RIGHT$("00000000"+Bin$(Peek(VAR MUS%(),MP)),8)
  SELECT CASE NESADR
   ' ENABLE FLAGS
   CASE &H4015:
    ChVolAnt(1)=-1    
    If MID$(M$,8,1)="0" THEN  
     EnableCh(1)=0: ChgChan(1)=1   'PULSE 1
     Counter(1)=0
    Else
     EnableCh(1)=1
    Endif
    ChVolAnt(2)=-1    
    If MID$(M$,7,1)="0" THEN
     EnableCh(2)=0: ChgChan(2)=1   'PULSE 2
     Counter(2)=0
    Else
     EnableCh(2)=1
    EndIf
    ChVolAnt(3)=-1    
    If MID$(M$,6,1)="0" THEN
     EnableCh(3)=0: ChgChan(3)=1   'TRIANGLE
     Counter(3)=0
    Else
     EnableCh(3)=1
    EndIf
    ChVolAnt(4)=-1    
    If MID$(M$,5,1)="0" THEN
     EnableCh(4)=0: ChgChan(4)=1   'NOISE
     Counter(4)=0
    Else
     EnableCh(4)=1
    EndIf

   ' FRAME COUNTER
   CASE &H4017:
    FrameMode=VAL(LEFT$(M$,1))  
    FrameInt=VAL(LEFT$(M$,1))
    '? "Frame Mode:" ; FrameMode ; " - Frame Int:"; FrameInt
    SELECT CASE FrameMode
     CASE 0:
      if FrameInt=0 then 
       SubSweep=120: SubEnv=240
      else
       SubSweep=60: SubEnv=60
      endif
     CASE 1: SubSweep=96: SubEnv=192
    END SELECT

   ' -- PULSE WAVES REGISTERS --

   ' VOLUME / LENGTH COUNTER HALT FLAG
   CASE &H4000, &H4004:
    T=(NESADR-&H3FFF): IF T=5 THEN T=2
    HaltFlag(T)=Val(MID$(M$,3,1))         ' SET HALT FLAG
    ChVol(T)=Val("&B"+RIGHT$(M$,4))*1.6+1 ' SET VOLUME
    BkpVol(T)=ChVol(T)
    If MID$(M$,4,1)="0" then
     ConstVol(T)=0 ' VOLUME / ENVELOPE
    else
     ConstVol(T)=1 ' CONSTANT VOLUME
    EndIf
    ' 12.5% AND 25% CYCLE DUTY SIMULATION USING SAWTOOTH WAVE 
    If Val("&B"+LEFT$(M$,2))<>2 then ChType$(T)="W" else ChType$(T)="Q"
    ChgChan(T)=1
   ' FREQUENCY - TIMER LOW
   CASE &H4002: TonB$(1)=M$: ChgChan(1)=1
   CASE &H4006: TonB$(2)=M$: ChgChan(2)=1
   ' FREQUENCY - TIMER HIGH / 5-BIT COUNTER
   CASE &H4003, &H4007:
    T=(NESADR-&H4002): IF T=5 THEN T=2
    TonA$(T)=RIGHT$(M$,3)
    Counter(T)=NESLTAB(Val("&B"+LEFT$(M$,5)))
    StartFlag(T)=1: ChgChan(T)=1
   
   ' -- TRIANGE WAVE REGISTERS --
 
   ' FREQUENCY - TIMER LOW
   CASE &H400A: TonB$(3)=M$: ChgChan(3)=1
   ' FREQUENCY - TIMER HIGH / 5-BIT COUNTER
   CASE &H400B: 
    TonA$(3)=RIGHT$(M$,3)
    T=NESLTAB(Val("&B"+LEFT$(M$,5)))
    If T>Counter(3) then Counter(3)=T
    ChgChan(3)=1
   ' LENGTH COUNTER HALT FLAG / 7-BIT COUNTER
   CASE &H4008:
    HaltFlag(3)=Val("&B"+LEFT$(M$,1))
    T=Val("&B"+RIGHT$(M$,7))
    If T>Counter(3) then Counter(3)=T
    If Counter(3)=&H80 then ChVol(3)=1
    ChgChan(3)=1

   ' -- NOISE CHANNEL REGISTERS --
   CASE &H400C:
    HaltFlag(4)=Val(MID$(M$,3,1))         ' SET HALT FLAG
    ChVol(4)=Val("&B"+RIGHT$(M$,4))*1.6+1 ' SET VOLUME
    BkpVol(4)=ChVol(4)
    If MID$(M$,4,1)="0" then
     ConstVol(4)=0 ' VOLUME / ENVELOPE
    else
     ConstVol(4)=1 ' CONSTANT VOLUME
    EndIf
    ChgChan(4)=1
   CASE &H400E:
    TonA$(4)=RIGHT$(M$,4) ' TIMER PERIOD
    if LEFT$(M$,1)="1" THEN ChType$(4)="W" else ChType$(4)="N" ' MODE FLAG
    ChgChan(4)=1
   CASE &h400F:
    Counter(4)=NESLTAB(Val("&B"+LEFT$(M$,5))) ' 5-BIT COUNTER
    StartFlag(4)=1: ChgChan(4)=1': ? LEFT$(M$,5);" - ";Counter(4)
  END SELECT

  ' PLAY PULSE CHANNELS  
  For T=1 to 2
   if HaltFlag(T)=0 then
    Counter(T)=Counter(T)-((TIMER-NESCNTSUB)/SubEnv)*10
    If Counter(T)<0 then Counter(T)=0': ChVol(T)=BkpVol(T)
   Endif
   If ConstVol(T)=0 then 
    ChVol(T)=ChVol(T)-((TIMER-NESCNTSUB)/SubEnv)*10
    If ChVol(T)<1 then ChVol(T)=BkpVol(T)    
   endif
   if ChgChan(T)=1 and EnableCh(t)=1 then
    ChgChan(T)=0
    NESTIMER=Val("&B"+TonA$(T)+TonB$(T))
    NESFREQ=NESCLK/(16*(NESTIMER+1))   
    IF NESFREQ>20000 THEN NESFREQ=20000
    IF NESFREQ<>TonAnt(T) or ChVol(T)<>ChVolAnt(T) then
     TonAnt(T)=NESFREQ: ChVolAnt(T)=ChVol(T)
     if ChVol(T)>=1 and NESTIMER>8 and (Counter(T)>0 or ConstVol(T)=1 or HaltFlag(T)=1) THEN
      SELECT CASE ChType$(T)
       CASE "Q": Play Sound T,B,Q,NESFREQ,ChVol(T)
       CASE "W": Play Sound T,B,W,NESFREQ,ChVol(T)
      END SELECT
     EndIf
    endif
   endif
   'if (Counter(T)<=0 and ConstVol(T)=0 and HaltFlag(T)=0) or ChVol(T)<=1 or NESTIMER<=8 or EnableCh(T)=0 then
   if (Counter(T)<=0) or ChVol(T)<=1 or NESTIMER<=8 or EnableCh(T)=0 then
    Play Sound T,B,O,1,1
   endif
  Next T

  ' PLAY TRIANGE CHANNEL
  If HaltFlag(3)=0 then 
   Counter(3)=Counter(3)-((TIMER-NESCNTSUB)/SubEnv)*250
   If Counter(3)<0 then Counter(3)=0
  Endif
  NESTIMER=Val("&B"+TonA$(3)+TonB$(3))
  if EnableCh(3)=1 and (Counter(3)>0 or HaltFlag(3)=1) and NESTIMER >8 then
   NESFREQ=NESCLK/(32*(NESTIMER+1))
   IF NESFREQ>20000 THEN NESFREQ=20000
   if ChgChan(3)=1 then Play Sound 3,B,T,NESFREQ,25
   ChgChan(3)=0
  else  
   Play Sound 3,B,O,1,1
  endif
 
  ' PLAY NOISE CHANNEL
  if HaltFlag(4)=0 then
   Counter(4)=Counter(4)-((TIMER-NESCNTSUB)/SubEnv)*25
   If Counter(4)<0 then Counter(4)=0
  Endif
  If ConstVol(4)=0 then    
   'ChVol(4)=ChVol(4)-((TIMER-NESCNTSUB)/SubEnv)*25
   ChgChan(4)=1
  endif
  if ChgChan(4)=1 and EnableCh(4)=1 then
   ChgChan(4)=0
   NESFREQ=(Val("&B"+TonA$(4))^1.1)-1
   IF NESFREQ<1 THEN NESFREQ=1
   'IF NESFREQ<>TonAnt(4) or ChVol(4)<>ChVolAnt(4) then
   ' TonAnt(4)=NESFREQ: ChVolAnt(4)=ChVol(4)
    T=ChVol(4)'-(ChVol(1)+ChVol(2))/5
    if T>=1 and Counter(4)>0 THEN    
     if ChType$(4)="N" then Play Sound 4,B,N,NESFREQ,T else Play Sound 4,B,W,NESFREQ,T
    EndIf
   'endif   
  endif
  if Counter(4)<=0 or ChVol(4)<=2 or EnableCh(4)=0 then
   Play Sound 4,B,O,1,1
  endif

  NESCNTSUB=TIMER
 
 ' ** VGM PAUSE **
 CASE &H61: PTim=PTim+SWait*(Peek(VAR Mus%(),MP+2)*256+Peek(VAR Mus%(),MP+1)): MP=MP+2
 CASE &H62: PTim=PTim+W60
 CASE &H63: PTim=PTim+W50
 CASE &H70 TO &H7F: Frames=(H-&H6F):PTim=PTim+SWait*Frames
 
 ' ** FUTURE IMPLEMENTATION **
 CASE &H66: MP=Loff  'STOP STREAM (OLD)
 CASE &H67:          'DATA BLOCKS - JUMP NOW 
  'MP=MP+3
  'MUSTMP$=HEX$(Peek(VAR MUS%(),MP))+HEX$(Peek(VAR MUS%(),MP+1))+HEX$(Peek(VAR MUS%(),MP+2))+HEX$(Peek(VAR MUS%(),MP+3))
  'MP=(Val("&H"+MUSTMP$))/8
  '? "Jumping ... ";(Val("&H"+MUSTMP$))/8
 CASE &H68: MP=MP+11 'PCM RAM WRITES
 CASE &H90: MP=MP+4  'SETUP STREAM CONTROL
 CASE &H91: MP=MP+4  'SET STREAM DATA
 CASE &H92: MP=MP+5  'SET STREAM FREQUENCY
 CASE &H93: MP=MP+10 'START STREAM
 CASE &H94: MP=MP+1  'STOP STREA CASE &H95: MP=MP+4  'START STREAM (FAST CALL

 ' ** JUMPS **
 CASE &H30 TO &H3F, &H40 TO &H4E: MP=MP+1
 CASE &HA1 TO &HAF, &HBC TO &HBF: MP=MP+2
 CASE &HC5 TO &HCF, &HD5 TO &HDF: MP=MP+3
 CASE &HE1 TO &HFF: MP=MP+4
 END SELECT
End Sub




