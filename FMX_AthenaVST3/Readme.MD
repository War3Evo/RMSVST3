# Delphi 11.0 (FMX_Athena)

## 11/20/2022
- I have HALTED working on this VCL to FMX as I hit a wall and can't figure it out.
- During my conversion from VCL to FMX... I found a problem I can't solve, so that being said.
I'm going back to the VCL version so that I can work on plug-ins, instead of working to try and
figure a work around that is above my head.  Someone mentioned using CreateWindowEX to do it, 
but I can't wrap my brain around on how to do that when you creating the form already?
can't seem to find a work around VCL CreateParented for unit UVST3Controller;
in function TVST3Controller.CreateForm(parent:pointer):TForm;

## 11/19/2022
- https://en.delphipraxis.net/topic/7856-why-is-fscrollbarsisbvaluesnum-causing-fmx-to-crash-open-source-vcl-version-doesnt-crash/
- ^ WriteLog uses CodeSite Express... CakeWalk by BandLabs crashes on FScrollBars[isb].Value:=sNum;
- ^ The VCL version does not cra﻿sh but also uses FScrollBars[isb].Position:=round(100*value); 

## 11/16/2022
- Still converting Athena to FMX

## 11/15/2022
- Converting Athena and all sub directories to FMX so that I can enable Skia4Delphi which would improve graphics performance by 50%

### Athena for VST3 Roadmap 11/12/2022
- Testing and Development in Cakewalk by Bandlabs
- Suggest Chords based on music theory
- Able to use basic synth or pick a channel for your own synth
- Sync with DAW
- Drag and Drop Chords to DAW or Chords Editor
- Customizable Chord suggestions database with strength indicator
- Ideas are welcomed: https://github.com/War3Evo/RMSVST3/discussions/categories/ideas

## 11/12/2022
- Created folder for Athena for VST3.  (currently just a synth copy)

## 11/9/2022
- Added CodeSiteLogging back into the source code
You can download CodeSiteLogging Express for free from GetIt Package Manager

- Fixed crash when closing the VST instrument window

## 11/6/2022
- Not everyone has CodeSiteLogging (removed / replaced with ULogger)
- renamed UCodeSiteLogger to ULogger
- fixed [dcc32 Warning] W1030 Invalid compiler directive: 'true' by changing it to 'Debug Information'
- remove .local as it does not need to be tracked in git
- added gitignore