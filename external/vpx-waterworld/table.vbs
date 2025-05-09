'**** Waterworld Gottlieb 1995 ***
'**** Original VP9 table by mfuegemann ****
'**** VPX Conversion by Goldchicco & 32assassin ****
'**** VPX Mod by Aubrel ****

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="waterwld",UseSolenoids=2,UseLamps=0,UseGI=0,SCoin="coin"
Const BallSize = 50

'Map Switch Numbers
Const swStartButton	=4
Const swTournament	=5  'LeftMagnaSave (LCtrl)
Const swFrontDoor	=6
Const BuyInButton 	=3  'Key "2"

LoadVPM "01560000","GTS3.VBS",3.2

'-----------------------------------
'------  Global Cofigurations ------
'-----------------------------------
Const EnableBerserkerBumper = 1	'Set to 1 to open wall between the Berserker and the lower Bumper to give acces to the lower Bumper.
Const OutLanePostSafe = 1		'Set to 1 for safe and to 0	for normal position of the two outlane posts

' Volume Options (Volume constants, 1 is max)
Const VolFlip    = 1     ' Flipper volume.
Const VolMotor   = 0.5   ' BigFatso motor volume
Const VolSling   = 1     ' Slingshot volume.
Const VolBump    = 1     ' Bumper volume.

' Sound Options (Volume multipliers)
Const VolRol     = 0.25  ' Ballrolling volume.
Const VolRamp    = 0.5   ' Ramps volume factor.
Const VolFlipHit = 10    ' Flipper colision volume.
Const VolRub     = 5     ' Rubbers/Posts colision volume.
Const VolPin     = 20    ' Rubber Pins colision volume.
Const VolMetal   = 10    ' Metals colision volume.
Const VolWood    = 10    ' Wood colision volume.
Const VolTarg    = 1000  ' Targets volume.
Const VolCol     = 2000  ' Ball collision volume.

'Visual Options
Dim Luts,LutPos
Luts = array("ColorGradeLUT256x16_1to1","ColorGradeLUT256x16_ModDeSat","ColorGradeLUT256x16_ConSat","ColorGradeLUT256x16_ExtraConSat","ColorGradeLUT256x16_ConSatMD","ColorGradeLUT256x16_ConSatD")
LutPos = 3         		 'set the nr of the LUT you want to use (0=first in the list above, 1=second, etc; 4 and 5 are "Medium Dark" and "Dark" LUTs); 3 is the default
Const VisualLock = 0     'set to 1 if you don't want to change the visual settings using Magna-Save buttons; 0 is the default (live changes enabled)


'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
'PINMAME has no ZERO ID support add 1  from service manual 

'Sol1  Left Bottom Bumper
'Sol2  Left Top Bumper
'Sol3  Right Bumper
'Sol4  not used
'Sol5  Left Slingshot
'Sol6  Right Slingshot
SolCallback(7)="SolTopUpKicker"						'Sol6  Top Upkicker
SolCallback(8)="SolBottomUpKicker"					'Sol7  Bottom Upkicker
SolCallback(9)="bsSaucer.SolOut"  					'Sol8  Left Ball Shooter
'Sol10  not used
SolCallback(11)="DTBank.SolDropUp"					'Sol10 2 Position Bank Reset 
SolCallback(12)="SetLamp 111,"						'Sol11 Wave Ramp Flasher
SolCallback(13)="SetLamp 112,"						'Sol12 Dive Hole Flasher
SolCallback(14)="SolMoveRamp"						'Sol13 Moving Ramp
SolCallback(15)="SolBallGate"						'Sol14 Ball Gate
SolCallback(16)="SolPivotGate"						'Sol15 Pivot Gate
SolCallback(17)="SetF16"							'Sol16 Left Card Holder Flasher
SolCallback(18)="SetF17"							'Sol17 Left Center Flasher
SolCallback(19)="SetF18"							'Sol18 Left Top Flasher
SolCallback(20)="SetLamp 119,"						'Sol19 Lightstrip Flasher
'Do not use PINMAME IDs 120-125, Ids are already used by PINMAME for the Aux Driver Board
SolCallback(21)="SetF20"							'Sol20 Right Top Flasher
SolCallback(22)="SetF21"							'Sol21 Right Center Flasher
SolCallback(23)="SetF22"							'Sol22 Right Cardholder Flasher
'Sol24 not used
SolCallback(25)="SolShipMotor"						'Sol24 Ship Motor
'Sol26 Lightbox Insert Illum. Relay 
'Sol27 Coin Meter enable
SolCallback(28)="bsTrough.SolOut"     				'Sol27 Ball Release	
SolCallback(29)="bsTrough.SolIn"    				'Sol28 Outhole
SolCallback(30)="vpmSolSound SoundFX(""Knocker"",DOFKnocker)," 'Sol29 Knocker
SolCallback(31)="PFGI" 								'Sol30 Tilt Relay  aka Gottlieb PFGI controller
SolCallback(32)="SolGameOn"							'Sol31 Game Over Relay	

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFFlippers),LeftFlipper,VolFlip:LeftFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFFlippers),LeftFlipper,VolFlip:LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySoundAtVol SoundFX("fx_Flipperup",DOFFlippers),RightFlipper,VolFlip:RightFlipper.RotateToEnd
     Else
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFFlippers),RightFlipper,VolFlip:RightFlipper.RotateToStart
     End If
End Sub

'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolBallGate(enabled)
	if enabled then
		BallGate.rotatetoend
		BallGate1.rotatetoend			'this one is used only to prevent stuck balls
		PlaySound SoundFX("fx_Flipperup",DOFContactors)
	else
		BallGate.rotatetostart
		BallGate1.rotatetostart
		PlaySound SoundFX("fx_Flipperdown",DOFContactors)
	end if
End Sub

Sub SetF16(Enabled)
	SetLamp 116, Enabled
	Flasher116.Visible=Enabled
End Sub

Sub SetF17(Enabled)
	SetLamp 117, Enabled
	Flasher117.Visible=Enabled
End Sub

Sub SetF18(Enabled)
	SetLamp 118, Enabled
	Flasher118.Visible=Enabled
End Sub

Sub SetF20(Enabled)
	SetLamp 126, Enabled
	Flasher220.Visible=Enabled
End Sub

Sub SetF21(Enabled)
	SetLamp 127, Enabled
	Flasher221.Visible=Enabled
End Sub

Sub SetF22(Enabled)
	SetLamp 128, Enabled
	Flasher222.Visible=Enabled
End Sub



'Playfield GI
Sub PFGI(Enabled)
	SetLamp 134, Enabled	'Backwall bulbs
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 0: Next
        PlaySound "fx_relay"
		LightsLevelUpdate 1, 0
	Else
		For each xx in GI:xx.State = 1: Next
        PlaySound "fx_relay"
		LightsLevelUpdate -1, 0
	End If
End Sub

Sub SolGameOn(enabled)				'Watchdog to correct misaligned ship at game start	
	if enabled then
		if DeezIsUp then
			DeezIsUp = False
			ShipTimer.enabled = True
			Atoll1.isdropped = DeezIsUp
			Atoll2.isdropped = DeezIsUp
			Deez1.isdropped = not DeezIsUp
			Deez2.Timerenabled = not DeezIsUp
			controller.switch(33) = 0			
		end if
	end if
	'FlipperActive = enabled
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsBottomUpKicker, bsSaucer, DTBank

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = ""&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		'.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0

    PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled = true

    vpmNudge.TiltSwitch=swTilt
    vpmNudge.Sensitivity=2
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftslingShot,RightslingShot) 

    Set bsTrough=New cvpmBallStack
        bsTrough.InitSw 31,0,0,32,0,0,0,0
        bsTrough.InitKick BallRelease,90,5
        bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
        bsTrough.Balls=3
		bsTrough.AddBall 0

    Set bsBottomUpKicker=New cvpmBallStack       
		bsBottomUpKicker.InitSw 0,50,0,0,0,0,0,0
        bsBottomUpKicker.Initkick BottomUpKicker,181,5
        bsBottomUpKicker.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsBottomUpKicker.KickBalls = 4
		bsBottomUpKicker.Balls=0

	Set bsSaucer=New cvpmBallStack       
		bsSaucer.InitSaucer LeftBallShooter,30,0,20
        bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		'bsSaucer.KickAngleVar=30
		bsSaucer.KickForceVar = 5

	set DTBank = new cvpmDropTarget
		DTBank.InitDrop Array(DT_25,DT_35), Array(25,35)
		DTBank.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	'Init Moving Ramp
	RampIdle = True
	Rampdiverter.isdropped = True
	RampDiverter.Timerenabled = True

	'Init Ship
	DeezIsUp = False
	controller.switch(33) = 1
	Deez1.isdropped = not DeezIsUp
	Atoll1.isdropped = DeezIsUp
	Atoll2.isdropped = DeezIsUp

	'Init Watercannon
	sw13Fire.isdropped = True
	sw13.isdropped = False

	If LutPos = 4 Then LightsLevelUpdate 1, 1    'set lights for "Medium Dark" LUT
	If LutPos = 5 Then LightsLevelUpdate 2, 2    'set lights for "Dark" LUT
	Table1.ColorGradeImage = Luts(LutPos)
End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftMagnaSave then Controller.switch(5) = 1     'Tournament (Left MagnaSave Button)
	If keycode = StartGamekey then Controller.switch(4) = 1		 '1 = Start
	If keycode = LeftFlipperKey Then Controller.switch(42) = 1
	If keycode = RightFlipperKey Then Controller.switch(42) = 1
    If keycode = keyFront Then KeyDownHandler(AddCreditKey2)    '2 = EB BuyIn
	If VisualLock = 0 Then
		If KeyCode = RightMagnaSave Then
			If LutPos = 5 Then LutPos = 0:LightsLevelUpdate -2,-2 Else LutPos = LutPos +1   'Max LUTs number is set to 5; so back to 0 and Lights level reseted for standard LUTs.
			If LutPos > 3 Then LightsLevelUpdate 1,1                                        'LUTs 4 and 5 are "Medium Dark" and "Dark LUTs" so Lights Level should be updated
			Table1.ColorGradeImage = Luts(LutPos)
		End If
	End If
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = LeftMagnaSave then Controller.switch(5) = 0
	If keycode = StartGamekey then Controller.switch(4) = 0	
	If keycode = LeftFlipperKey Then Controller.switch(42) = 0
	If keycode = RightFlipperKey Then Controller.switch(42) = 0

	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsound"drain" : End Sub
Sub LeftBallShooter_Hit:bsSaucer.addball me : playsound "popper_ball": End Sub

'Drop Targets
Sub DT_25_Dropped:DTBank.Hit 1:End Sub
Sub DT_35_Dropped:DTBank.Hit 2:End Sub

'Bumpers:
Sub Bumper1_hit:vpmTimer.PulseSw 10 : PlayBumperSound Bumper1 : End Sub
Sub Bumper2_hit:vpmTimer.PulseSw 11 : PlayBumperSound Bumper2 : End Sub
Sub Bumper3_hit:vpmTimer.PulseSw 12 : PlayBumperSound Bumper3 : End Sub

Sub PlayBumperSound(BumperNr)
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtBumperVol SoundFX("fx_bumper_1",DOFContactors),BumperNr,VolBump
			Case 2 : PlaySoundAtBumperVol SoundFX("fx_bumper_2",DOFContactors),BumperNr,VolBump
			Case 3 : PlaySoundAtBumperVol SoundFX("fx_bumper_3",DOFContactors),BumperNr,VolBump
		End Select
End Sub

'Wire Triggers
sub sw20_hit:Controller.Switch(20)=1:End Sub		'20
sub sw20_unhit:Controller.Switch(20)=0:End Sub

sub sw60_hit:Controller.Switch(60)=1:End Sub		'60
sub sw60_unhit:Controller.Switch(60)=0:End Sub
sub sw70_hit:Controller.Switch(70)=1:End Sub		'70
sub sw70_unhit:Controller.Switch(70)=0:End Sub

sub Trigger51_hit:Controller.Switch(51)=1:End Sub		'51 Wave entry
sub Trigger51_unhit:Controller.Switch(51)=0:End Sub	
sub Trigger61_hit:Controller.Switch(61)=1:End Sub		'61	Wave made (or on its way)
sub Trigger61_unhit:Controller.Switch(61)=0:End Sub	
sub Trigger40_hit:Controller.Switch(40)=1:End Sub		'40
sub Trigger40_unhit:Controller.Switch(40)=0:End Sub

sub sw74_hit:Controller.Switch(74)=1:PlaySound "Gate2",0,1,0.15,0.1:End Sub		'70
sub sw74_unhit:Controller.Switch(74)=0:End Sub

sub sw53_hit:Controller.Switch(53)=1:PlaySound "Gate2",0,1,-0.18,0.1:End Sub		'42
sub sw53_unhit:Controller.Switch(53)=0:End Sub		
sub sw63_hit:Controller.Switch(63)=1:PlaySound "Gate2",0,1,-0.17,0.1:End Sub		'43
sub sw63_unhit:Controller.Switch(63)=0:End Sub		

sub sw54_hit:Controller.Switch(54)=1:PlaySound "Gate2",0,1,0.18,0.1:End Sub		'42
sub sw54_unhit:Controller.Switch(54)=0:End Sub		
sub sw64_hit:Controller.Switch(64)=1:PlaySound "Gate2",0,1,0.16,0.1:End Sub		'44		
sub sw64_unhit:Controller.Switch(64)=0::End Sub		

'Kicking Target
Sub sw13_Slingshot
	vpmTimer.PulseSw 13
	PlaySoundAt SoundFX("Popper",DOFContactors),sw13
	sw13.isdropped = True
	sw13Fire.isdropped = False
	sw13Fire.timerenabled = True
End Sub

Sub sw13Fire_Timer
	sw13Fire.timerenabled = False
	sw13Fire.isdropped = True
	sw13.isdropped = False
End Sub

'***** Targets ***********
Sub sw55_Hit:vpmTimer.PulseSw 55:Playsound SoundFX("Target",DOFTargets),0,1,0.15,0.25:End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:Playsound SoundFX("Target",DOFTargets),0,1,0.17,0.25:End Sub
Sub sw65_Hit:vpmTimer.PulseSw 65:Playsound SoundFX("Target",DOFTargets),0,1,0.15,0.25:End Sub
Sub sw66_Hit:vpmTimer.PulseSw 66:Playsound SoundFX("Target",DOFTargets),0,1,0.16,0.25:End Sub
Sub sw76_Hit:vpmTimer.PulseSw 76:Playsound SoundFX("Target",DOFTargets),0,1,0.15,0.25:End Sub


'LightsLevelUpdate by Aubrel
Dim obj
Sub LightsLevelUpdate(OffsetL, OffsetGI)
  For each obj in PFLights
    obj.Intensity = obj.Intensity * 1.3^(OffsetL)
  Next
  For each obj in FlasherLights
    obj.Intensity = obj.Intensity * 1.15^(OffsetL)
  Next
  For each obj in OtherLights
    obj.Intensity = obj.Intensity * 1.15^(OffsetL)
  Next
  For each obj in AllFlashers
    obj.Opacity = obj.Opacity * 1.2^(OffsetL)
  Next
  For each obj in GI
    obj.Intensity = obj.Intensity * 1.25^(OffsetGI)
  Next
End Sub

'---------------------------------------
'Berserker Pivot Gate
'---------------------------------------

Dim PivotGateUp
Sub SolPivotGate(enabled)
	if enabled then
		PivotGateUp = True
		PivoGateWall.Timerenabled = True
		'P_PivotGate.rotX = 38
		PlaySoundAt SoundFX("fx_Flipperdown",DOFContactors),P_PivotGate
	else
		PivotGateUp = False
		PivoGateWall.Timerenabled = True
		'P_PivotGate.rotX = 18
		PlaySoundAt SoundFX("fx_Flipperdown",DOFContactors),P_PivotGate
	end if	
end Sub

Sub PivoGateWall_Timer
	if PivotGateUp then
		P_PivotGate.rotX = P_PivotGate.rotX + 1
	else
		P_PivotGate.rotX = P_PivotGate.rotX - 1
	end if
	if P_PivotGate.rotX < 18 then
		PivoGateWall.Timerenabled = False
		P_PivotGate.rotX = 18
	end if	
	if P_PivotGate.rotX > 38 then
		PivoGateWall.Timerenabled = False
		P_PivotGate.rotX = 38
	end if	
End Sub

'---------------------------------------
'------  Wave Ramp  ------
'---------------------------------------

Sub Wall14_Hit
	if ActiveBall.Velz > 5 then
		ActiveBall.Velz = 0
		ActiveBall.z = 131
	end if
End Sub

Sub WaveMade_Hit
	if DeezIsUp then
		vpmtimer.pulsesw 51
		vpmTimer.AddTimer 100,"PulseSW61"
	end if
end sub

Sub PulseSW61(swNo)
	vpmtimer.pulsesw 61
End Sub

Sub Trigger1_Hit				'prevent the ball from jumping too high
	ActiveBall.Velz = 0
	ActiveBall.z = 131
End Sub

Sub Trigger2_Hit				'second check point
	ActiveBall.Velz = 0
	ActiveBall.z = 131
End Sub

'---------------------------------------
'------  Top Kicker  ------
'---------------------------------------

Sub VUK_Hit : Controller.Switch(22) = 1 : playsound "popper_ball": End Sub

Sub SolTopUpKicker(enabled)
	If Enabled Then
   	  If Controller.Switch(22) = True Then
		PlaySound SoundFX("Popper",DOFContactors)
		Controller.Switch(22) = 0
		VUK.destroyball
		VUK1.CreateBall
		vpmTimer.AddTimer 70,"VUKLevel1"
	  end if
	end if
end sub

Sub VUKLevel1(swNo)
	Controller.Switch(22)=0
	VUK1.DestroyBall
	VUK2.CreateBall
	vpmTimer.AddTimer 70,"VUKLevel2"
End Sub

Sub VUKLevel2(swNo)
	VUK2.DestroyBall
	VUK3.CreateBall
	vpmTimer.AddTimer 70,"VUKLevel3"
End Sub

Sub VUKLevel3(swNo)
	VUK3.DestroyBall
	VUK4.CreateBall
	vpmTimer.AddTimer 70,"VUKLevel4"
End Sub

Sub VUKLevel4(swNo)
	VUK4.DestroyBall
	VUKTop.CreateBall
	VUKTop.Kick 160,5
End Sub

'---------------------------------------
'------  Bottom Kicker  ------
'---------------------------------------
'Bottom Kicker
Sub BottomUpKicker_Hit:bsBottomUpKicker.addball me : playsound "popper_ball": End Sub

Sub SolBottomUpKicker(enabled)
	if enabled then
		bsBottomUpKicker.ExitSol_On
		BottomUpKicker.Timerenabled = True
		PlaySound SoundFX("Popper",DOFContactors)
	end if
End Sub

'Bottom UpKicker
Sub sw50_Hit
	if not BottomUpKicker.Timerenabled then 
		sw50.destroyball
		bsBottomUpKicker.addball 0
		Playsound "Drain7",0,1,0.05,0.25		

	end if
End Sub

Sub BottomUpKicker_Timer
	BottomUpKicker.Timerenabled = False
End Sub


'---------------------------------------
'------  Moving Ramp  ------
'---------------------------------------
Dim RampIdle
RampIdle = False

Sub SolMoveRamp(enabled)
	if enabled then
		RampIdle = False
		Rampdiverter.isdropped = False
		RampDiverter.Timerenabled = True 

	else
		RampIdle = True
		Rampdiverter.isdropped = True
		RampDiverter.Timerenabled = True

	end if
End Sub

Sub RampDiverter_Timer
	PlaySoundAtVol SoundFX("Motor",DOFContactors),P_MovingRamp,VolMotor
	if RampIdle then
		P_MovingRamp.ObjRotZ = P_MovingRamp.ObjRotZ - 1
		P_MovingRampTopper.ObjRotZ = P_MovingRampTopper.ObjRotZ - 1
		if P_MovingRamp.ObjRotZ < 2 then
			RampDiverter.Timerenabled = False
			P_MovingRamp.ObjRotZ = 1
			P_MovingRampTopper.ObjRotZ = 1
		end if
	else
		P_MovingRamp.ObjRotZ = P_MovingRamp.ObjRotZ + 1
		P_MovingRampTopper.ObjRotZ = P_MovingRampTopper.ObjRotZ + 1
		if P_MovingRamp.ObjRotZ > 45 then
			RampDiverter.Timerenabled = False
			P_MovingRamp.ObjRotZ = 46
			P_MovingRampTopper.ObjRotZ = 46
		end if
	end if
End Sub

'---------------------------------------
'------Rotating Ship------
'---------------------------------------
Dim DeezIsUp
DeezIsUp = False

Dim MotorIsRunning
MotorIsRunning = False

Sub SolShipMotor(enabled)
	if enabled then
'		DOF 101, DOFOn
		MotorIsRunning = not MotorIsRunning		
		if MotorIsRunning then
			DeezIsUp = not DeezIsUp
			ShipTimer.enabled = True
			Atoll1.isdropped = DeezIsUp
			Atoll2.isdropped = DeezIsUp
			Deez1.isdropped = not DeezIsUp
			controller.switch(33) = 0
			if DeezIsUp then
				Deez2.isdropped = False
			else
				Deez2.Timerenabled = True
			end if
		end if
	end if
End Sub

Sub Deez2_Timer						'Ball Guide for better visuals
	Deez2.Timerenabled = False
	Deez2.isdropped = True
End Sub

Sub ShipTimer_Timer
	PlaySoundAtVol SoundFX("Motor",DOFContactors),P_Ship,VolMotor
	P_Ship.rotx = P_Ship.rotx - 1
	if P_Ship.rotx < 1 then									'Deez
		ShipTimer.enabled = False
		P_Ship.rotx = 360
		controller.switch(33) = 0
		MotorIsRunning = False
'		DOF 101, DOFOff
	else		
		if P_Ship.rotx < 181 and P_Ship.rotx > 179 then		'Atoll
			ShipTimer.enabled = False	
			P_Ship.rotx = 180
			controller.switch(33) = 1
			MotorIsRunning = False
'			DOF 101, DOFOff
		end if
	end if
End Sub


'Shooter Lane
Dim ShootVel
Sub ShooterLaneLaunch_Hit
	if ActiveBall.vely < -6 then PlaySound "Launch",0,1,0.25,0.25		
End Sub

Sub ShooterLaneTop_Hit
	ShootVel = SQR((ActiveBall.velx^2) + (ActiveBall.vely^2))
	if ShootVel > 6 then
		ShooterLaneTop.destroyball
		ShooterKicker.Timerenabled = True
	end if
End Sub

Sub ShooterKicker_Timer
	ShooterKicker.Timerenabled = False
	ShooterKicker.createball
	ShooterKicker.kick 180,ShootVel * 0.45
End Sub	

'Left Ball Shooter
Dim LShootVel

Sub LaftShooterTop_Hit
	LShootVel = SQR((ActiveBall.velx^2) + (ActiveBall.vely^2))
	if ShootVel > 6 then
		LaftShooterTop.destroyball
		LeftBallShooterKicker.Timerenabled = True
	end if
End Sub

Sub LeftBallShooterKicker_Timer
	LeftBallShooterKicker.Timerenabled = False
	LeftBallShooterKicker.createball
	LeftBallShooterKicker.kick 180,ShootVel * 0.45
End Sub	



'Apply Global Settings
Sub ApplyGlobalSettings
	if OutlanePostSafe then
		ROutlanePost_Normal.isdropped = True
		LOutlanePost_Normal.isdropped = True
		ROutlanePost_Safe.isdropped = False
		LOutlanePost_Safe.isdropped = False
		P_ROutlanePost.TransY = 0
		P_LOutlanePost.TransY = 0
	else
		ROutlanePost_Normal.isdropped = False
		LOutlanePost_Normal.isdropped = False
		ROutlanePost_Safe.isdropped = True
		LOutlanePost_Safe.isdropped = True
		P_ROutlanePost.TransY = -15
		P_LOutlanePost.TransY = -15
	end if 

	if EnableBerserkerBumper then
		BerserkerBumperWall.isdropped = True
	else
		BerserkerBumperWall.isdropped = False
	end if	
End Sub



'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step

	   'Special Handling
	   'If chgLamp(ii,0) = 2 Then solTrough chgLamp(ii,1)
	   'If chgLamp(ii,0) = 4 Then PFGI chgLamp(ii,1)

        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps()
NFadeL 0, Lamp0
'NFadeL 1, Lamp1  'Credit Button
NFadeL 2, Lamp2
NFadeL 3, Lamp3
NFadeL 4, Lamp4
NFadeL 5, Lamp5
NFadeL 6, Lamp6
NFadeL 7, Lamp7
'NFadeL 8, Lamp8 'Not in the service manual
'NFadeL 9, Lamp9 'Not in the service manual
'NFadeL 10, Lamp10 'Not Used
NFadeL 11, Lamp11
NFadeObj 12, P_Lamp12, "Lamp12_lit", "Lamp12_unlit"
NFadeObj 13, P_Lamp13, "Lamp13_lit", "Lamp13_unlit"
NFadeObj 14, P_Lamp14, "Lamp14_lit", "Lamp14_unlit"
NFadeL 15, Lamp15
NFadeL 16, Lamp16
NFadeL 17, Lamp17
'NFadeL 18, Lamp18 'Not in the service manual
'NFadeL 19, Lamp19 'Not in the service manual
'NFadeL 20, Lamp20 'Not Used
'NFadeL 21, Lamp21 'Not Used
NFadeObj 22, P_Lamp22, "AddTime_lit", "AddTime_unlit"
NFadeObj 23, P_Lamp23, "AdvJackpot_lit", "AdvJackpot_unlit"
NFadeObj 24, P_Lamp24, "Lamp24_lit", "Lamp24_unlit"
NFadeL 25, Lamp25
NFadeL 26, Lamp26
NFadeL 27, Lamp27
'NFadeL 28, Lamp28 'Not in the service manual
'NFadeL 29, Lamp29 'Not in the service manual
'NFadeL 30, Lamp30 'Not Used
'NFadeL 31, Lamp31 'Not Used
NFadeObj 32, P_Lamp32,"TreasureChest_lit", "TreasureChest_unlit"
NFadeL 33, Lamp33
NFadeObj 34, P_Lamp34, "Lamp34_lit", "Lamp34_unlit"
NFadeObj 35, P_Lamp35, "Lamp35_lit", "Lamp35_unlit"
NFadeObj 36, P_Lamp36, "Lamp36_lit", "Lamp36_unlit"
NFadeObj 37, P_Lamp37, "Lamp37_lit", "Lamp37_unlit"
'NFadeL 38, Lamp38 'Not in the service manual
'NFadeL 39, Lamp39 'Not in the service manual
'NFadeL 40, Lamp40 'Not Used
'NFadeL 41, Lamp41 'Not Used
NFadeL 42, Lamp42
NFadeL 43, Lamp43
NFadeL 44, Lamp44
NFadeObj 45, P_Lamp45, "Lamp45_lit", "Lamp45_unlit"
NFadeObj 46, P_Lamp46, "Lamp46_lit", "Lamp46_unlit"
NFadeObj 47, P_Lamp47, "Lamp47_lit", "Lamp47_unlit"
'NFadeL 48, Lamp48 'Not in the service manual
'NFadeL 49, Lamp49 'Not in the service manual
'NFadeL 50, Lamp50 'Not Used
NFadeL 51, Lamp51
NFadeL 52, Lamp52
NFadeObj 53, P_Lamp53, "HYDRO_lit", "HYDRO_unlit"
NFadeObj 54, P_Lamp54, "Dryland_lit", "Dryland_unlit"
NFadeObj 55, P_Lamp55, "4Corners_lit", "4Corners_unlit"
NFadeLm 56, Lamp56
NFadeL 56, Lamp56b
NFadeL 57, Lamp57
'NFadeL 58, Lamp58 'Not in the service manual
'NFadeL 59, Lamp59 'Not in the service manual
NFadeObj 60, P_Lamp60, "Deez_lit", "Deez_unlit"
NFadeObj 61, P_Lamp61, "SpecialChoice_lit", "SpecialChoice_unlit"
NFadeObj 62, P_Lamp62, "Lamp62_lit", "Lamp62_unlit"
NFadeObj 63, P_Lamp63, "Lamp63_lit", "Lamp63_unlit"
NFadeObj 64, P_Lamp64, "Lamp64_lit", "Lamp64_unlit"
NFadeObj 65, P_Lamp65, "Lamp65_lit", "Lamp65_unlit"
NFadeObj 66, P_Lamp66, "Lamp66_lit", "Lamp66_unlit"
NFadeObjm 67, P_Lamp67, "Flasher_green", "Flasher_green_dark"
NFadeLm 67, L67  'LED
Flash 67, Flasher67
'68-69 'Not in the Service manual
NFadeL 70, L70  'Bumper 1
NFadeL 71, L71  'Bumper 2
NFadeL 72, L72  'Bumper 3

'73-77 'Not used

'BackWall LEDs mapped to GI Controller

Flashm 134, Flasher134a
Flashm 134, Flasher134b
Flashm 134, Flasher134c
Flashm 134, Flasher134d
Flashm 134, Flasher134e
Flashm 134, Flasher134f
Flashm 134, Flasher134g
Flashm 134, Flasher134h
Flashm 134, Flasher134i
Flash 134, Flasher134j

'Solenoid Controlled Flashers/Lights

NFadeLm 111, S111
NFadeLm 111, S111a
NFadeL 111, S111b

NFadeLm 112, S112
Flash 112, Flasher112

'NFadeObjm 116, P_Flasher16, "dome3_blue_o", "dome3_blue"
NFadeLm 116, S116
FadeDisableLighting 116, P_Flasher16, 40
'Flash 116, Flasher116

'NFadeObjm 117, P_Flasher17, "dome3_red_o", "dome3_red"
NFadeLm 117, S117
FadeDisableLighting 117, P_Flasher17, 20
'Flash 117, Flasher117

'NFadeObjm 118, P_Flasher18, "dome3_clear_o", "dome3_clear"
NFadeLm 118, S118
FadeDisableLighting 118, P_Flasher18, 10
'Flash 118, Flasher118

NFadeLm 119, S119
Flash 119, Flasher119

'NFadeObjm 126, P_Flasher20, "dome3_clear_o", "dome3_clear"
NFadeLm 126, S220
FadeDisableLighting 126, P_Flasher20, 10
'Flash 126, Flasher220

'NFadeObjm 127, P_Flasher21, "dome3_red_o", "dome3_red"
NFadeLm 127, S221
FadeDisableLighting 127, P_Flasher21, 20
'Flash 127, Flasher221

'NFadeObjm 128, P_Flasher22, "dome3_blue_o", "dome3_blue"
NFadeLm 128, S222
FadeDisableLighting 128, P_Flasher22, 40
'Flash 128, Flasher222

'Backglass Flahsers

'Sol25 - Backglass GI

'DRV 0 = 120
'DRV 1 = 121
'DRV 2 = 122 
'DRV 3 = 123 
'DRV 4 = 124
'DRV 5 = 125

End Sub


' div lamp subs
Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself
Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off
Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub


'FadeDisableLighting by iaakki
Sub FadeDisableLighting(nr, a, alvl)
  debug.print a.uservalue
  Select Case FadingLevel(nr)
    Case 4
      a.UserValue = a.UserValue - (a.UserValue * 0.5)^3 - 0.03
      If a.UserValue < 0 Then 
        a.UserValue = 0
        FadingLevel(nr) = 0
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
    Case 5
      a.UserValue = (a.UserValue + 0.1) * 1.1
      If a.UserValue > 1 Then 
        a.UserValue = 1
        FadingLevel(nr) = 1
      end If
      a.BlendDisableLighting = alvl * a.UserValue 'brightness
  End Select
End Sub


' Flasher objects
Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 15
    PlaySoundAtVol SoundFX("Right_Slingshot",DOFContactors),Sling1, VolSling
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
'	gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 14
    PlaySoundAtVol SoundFX("Left_Slingshot",DOFContactors),Sling2, VolSling
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
'	gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub



' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
  RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
  BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
  VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10 * VolRamp
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol*VolRol, AudioPan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000 * VolCol, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

	Prim_LeftFlipper.RotY=LeftFlipper.currentangle-90
	Prim_RightFlipper.RotY=RightFlipper.currentangle-90

End Sub

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPin, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub DropTargets_Hit (idx)
	RandomSoundFlipper()
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Wood_Hit (idx)
	PlaySound "fx_woodhit", 0, Vol(ActiveBall)*VolWood, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber_band", 0, Vol(ActiveBall)*VolRub, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber_post", 0, Vol(ActiveBall)*VolRub, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRub, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRub, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRub, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolFlipHit, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolFlipHit, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolFlipHit, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

' Ramps sounds
Sub RampSound1_Hit: PlaySound "fx_metalrolling1": End Sub
Sub RampSound2_Hit: PlaySound "fx_metalrolling1": End Sub
Sub RampSound3_Hit: PlaySound "fx_metalrolling1": End Sub
Sub RampSound4_Hit: PlaySound "fx_plasticrolling1": End Sub

' Stop Ramps Sounds
Sub RampSound5_Hit: StopSound "fx_metalrolling1": End Sub
Sub RampSound6_Hit: StopSound "fx_plasticrolling1": End Sub

Sub Table1_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub