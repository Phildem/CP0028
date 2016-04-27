'--------------------------------------------------------------
' CP0028 - Pump Supervisor Firmware
' File: CP0028.BAS
' By Philippe Demerliac
' (c) 2011 Cyrob
'------------------------------------------------------------------------------
' Version history
' Version Date          Author      Comment
'   1.0d1 26 Feb 2011   Phildem     First release And preliminary tests
'   1.0d2 27 Feb 2011   Phildem     Change Display mode
'   1.0d3 06 Mar 2011   Phildem     Final Timer And dispay Test
'   1.0d4 02 Jun 2011   Phildem     Io Alias And final logic implementation
'   1.0b1 03 Jun 2011   Phildem     Final Test
'   1.0   03 Jun 2011   Phildem     Final release
'   1.1   09 Mar 2013   Phildem     Fix Bug Sample on DeadMode
'------------------------------------------------------------------------------
' Hardware connexions

' P1 Use

' In PumpOn       P1.0  Activ low
' In PumpPwr      P1.1  Activ low
' In PumpOverHeat P1.2  Activ low
' Out PumpOff     P1.3  Activ low
' Out IndLight On P1.4  Activ low
' Out Siren On    P1.5  Activ low
' In TestMode     P1.6  Activ low

' P3 Use all as output

' SegBCD A  P3.0  LSB Positiv Logic
' SegBCD B  P3.1
' SegBCD C  P3.2
' SegBCD D  P3.3

' Anode LSB P3.4  Display On when low
' Anode MSB P3.5  Display On when low

' Dp        P3.7  Dp On If 1 Used as WatchDog trigger


'------------------------------------------------------------------------------
$regfile = "89C2051.dat"                            'Define machine
$crystal = 12000000                                 '12 MHz XTal


'Const declaration ------------------------------------------------------------
Const kDig_Delay = 1                                'Digit display Time in ms
Const kRelayPulseWidth = 300                        'Relay pulse width in ms

Const kSirenTrig = 25                               'Siren Trig in Mn
Const kPumpMaxOnMn = 30                             'Max On Time in Mn
Const kSirenOnTime = 30                             'Siren On Time in Ticks

Const kTickPerMn = 120                              'Normal Ticks/mn
Const kTickPerMnTst = 2                             'Test Ticks/mn

Const kPumpDeadTime = 60                            'Normal Dead Time in Ticks
Const kPumpDeadTimeTst = 10                         'Test Dead Time in Ticks

' Possible Engine States
Const kStateIdle = 0                                'Normal Idle
Const kStateOn = 1                                  'Pump is On
Const kStateDead = 2                                'Pump is Dead Time
Const kStatePwrOff = 3                              'Pump Power Manually Off
Const kStateTimeOut = 4                             'Pump Time Out
Const kStateOverHeat = 5                            'Pump Over Heat


'IO Aliasing ------------------------------------------------------------------

In_PumpOn Alias P1.0
In_PumpPwr Alias P1.1
In_OverHeat Alias P1.2
In_TestMode Alias P1.6

Out_PumpRelay Alias P1.3
Out_Indic Alias P1.4
Out_Siren Alias P1.5

'Var Decl ---------------------------------------------------------------------

Dim Clock_word As Word                              'Timer counter
Dim Tick_hsec As Bit                                'Every 500ms Tick flag
Dim Watchd As Byte                                  'Watchd toggle


Dim Cnt_mn As Byte                                  'Tick counter for 1 Mn
                                                    'will count 0->119

Dim Cnt_PumpOn As Byte                              'Pump On Time In Mn
Dim DigLsb As Byte                                  'Lsb Digit Value
Dim DigMsb As Byte                                  'Msb Digit Value
Dim OutP3 As Byte                                   'Port 3 output buffer
Dim CurDisp As Bit                                  'Curent disp: 0 Lsb,1Msb
Dim PumpOn As Byte                                  'Nbr of Tick since PumpOn
Dim OnBlink As Bit                                  'Dead Blinker
Dim SirenOn As Bit                                  'Siren is On
Dim State As Byte                                   'System State

Dim VTickPerMn As Byte                              'Used Tick/Mn
Dim VPumpDeadTime As Byte                           'Used Dead Time in Ticks


'Program ----------------------------------------------------------------------

OnReset:

'Configure Timer0 for Interupt every 250µs

Config Timer0 = Timer , Gate = Internal , Mode = 2

On Timer0 Timer0_Int

Load Timer0 , 250

Priority Set Timer0
Enable Interrupts
Enable Timer0


'Init IO
P3 = &B01110000                                     'Display Off

'Init var
Clock_word = 0                                      'Init Timer counter
Cnt_mn = 0
Cnt_PumpOn = 0                                      'Init On Counter
CurDisp = 0
Watchd = 0
PumpOn = 0
OnBlink = 0
State = kStateIdle

' Look for Test Mode
If In_TestMode = 0 Then
   VTickPerMn = kTickPerMnTst
   VPumpDeadTime = kPumpDeadTimeTst

   DigLsb = 8                                       'Disp u8 in Test Mode
   DigMsb = 12
else
   VTickPerMn = kTickPerMn
   VPumpDeadTime = kPumpDeadTime

   'Display c8 for 0.3 sec to test display
   DigLsb = 8                                       'Disp 88 in Test Mode
   DigMsb = 8
End If

'Run display
Start Timer0

Waitmse 300

'Blank display
DigLsb = 15
DigMsb = 15


'Switch Pump Power On If off And not Overheat
If In_PumpPwr = 1 And In_OverHeat = 1 Then
   Gosub PulsePump
Else
   If In_OverHeat = 0 Then
      State = kStateOverHeat
   End If
End If


' Main Loop -------------------------------------------------------------------
Do


If Tick_hsec = 1 Then                               'Every half Sec tick
   Tick_hsec = 0

   If Watchd = 0 Then
      Watchd = &B10000000                           'Toggle Watch dog
   Else
      Watchd = 0
   End If

   'Calc State
   If In_PumpPwr = 0 Then                           ' Look If Pwr Is On
                                'Look for over heat
      If In_OverHeat = 0 Then
          State = kStateOverHeat
      else
         If State > kStateDead Then                 ' Manually set Powered On
            State = kStateIdle
            Cnt_PumpOn = 0
            PumpOn = 0
         End If

         If In_PumpOn = 0 Then                      ' Pump is running
            State = kStateOn
            PumpOn = VPumpDeadTime

            If Cnt_PumpOn > kPumpMaxOnMn Then
               State = kStateTimeOut
            End If

         Else
            If PumpOn = 0 Then                      'Pump really stopped
               State = kStateIdle
               Cnt_PumpOn = 0                       'Reset On counter
            else
               State = kStateDead
               Decr PumpOn
            End If

         End If
      End If

   else

      If State < kStateTimeOut Then                 ' Look If manually Off
         State = kStatePwrOff
      End If

   End If

   SirenOn = 0
   If state = kStateOn Or State = kStateDead Then
      If Cnt_PumpOn > kSirenTrig Then
         SirenOn = 1
      End If
   End If

   Select Case State
      Case kStateIdle :                             'Idle, Blank display
         DigLsb = 15
         DigMsb = 15
         Set Out_Indic                              ' Indic Off

      Case kStateOn :                               'On Display Time
         DisTime:
         DigLsb = Cnt_PumpOn Mod 10                 ' Calc Lsb
         DigMsb = Cnt_PumpOn / 10                   ' Calc Msb
         Reset Out_Indic                            ' Indic On

      Case kStateDead:                              'Dead Blink Display
         OnBlink = Not OnBlink
         If OnBlink = 0 Then Goto DisTime
         DigLsb = 15
         DigMsb = 15
         Reset Out_Indic                            'Indic On

       Case kStatePwrOff:                           'Of Display c Rev c
         DigLsb = 11
         DigMsb = 10
         Out_Indic = Not Out_Indic

       Case kStateTimeOut:                          'Time Out Display tt
         DigLsb = 14
         DigMsb = 14
         Gosub OffCond

       Case kStateOverHeat:                         'Heat display t0
         DigLsb = 0
         DigMsb = 14
         Gosub OffCond

      Case Else :                                   ' Abnormal
         Goto OnReset
   End Select


   Incr Cnt_mn

   'Siren Handling
   If SirenOn = 1 And Cnt_mn <= kSirenOnTime Then
      Reset Out_Siren
   else
      Set Out_Siren
    End If

   ' Minute Timer
   If Cnt_mn = VTickPerMn Then
      Cnt_mn = 0

      If State = kStateOn Or State = kStateDead Then       'if pump is on, incr Cntr
         Incr Cnt_PumpOn
      End If

   End If


End If                                              'Of Tick


'And continue for ever, that's the computer hard duty..
Loop


'---------------------------------------------------------
' Off Cond  ----------------------------------------------
'---------------------------------------------------------
OffCond:

    'Force Pump Off
    If In_PumpPwr = 0 Then Gosub PulsePump

    'Togle Light indicator
    Out_Indic = Not Out_Indic

Return

'---------------------------------------------------------
' Pulse Pump  --------------------------------------------
'---------------------------------------------------------
PulsePump:
   Reset Out_PumpRelay
   Waitmse kRelayPulseWidth
   Set Out_PumpRelay
Return

'------------------------------------------------------------------------------
' Timer 0 Interrupt -----------------------------------------------------------
'------------------------------------------------------------------------------

Timer0_Int:

'All Display Off Phamtom effect killer
OutP3 = Watchd Or &B01110000
P3 = OutP3

'Incr Sec Ticks
Incr Clock_word
If Clock_word = 2000 Then
   Clock_word = 0
   Tick_hsec = 1
End If

'Refresh Display
If CurDisp = 1 Then
   OutP3 = Watchd Or DigMsb
   P3 = OutP3 Or &B01010000                         ' Display DigMsb
Else
   Outp3 = Watchd Or DigLsb
   P3 = OutP3 Or &B01100000                         ' Display DigLsb
End If

CurDisp = Not CurDisp                               ' Switch to next display

Return