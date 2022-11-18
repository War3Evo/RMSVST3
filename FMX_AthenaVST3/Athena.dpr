library Athena;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  UCDataLayer in 'FrameworkCommon\UCDataLayer.pas',
  ULogger in 'FrameworkCommon\ULogger.pas',
  UVSTBase in 'FrameworkCommon\UVSTBase.pas',
  UCAudioProcessor in 'VST3SDK\UCAudioProcessor.pas',
  UCComponent in 'VST3SDK\UCComponent.pas',
  UCEditController in 'VST3SDK\UCEditController.pas',
  UCMidiMapping in 'VST3SDK\UCMidiMapping.pas',
  UCPluginFactory in 'VST3SDK\UCPluginFactory.pas',
  UCUnitInfo in 'VST3SDK\UCUnitInfo.pas',
  UVST3Controller in 'VST3SDK\UVST3Controller.pas',
  UVST3Processor in 'VST3SDK\UVST3Processor.pas',
  UVST3Utils in 'VST3SDK\UVST3Utils.pas',
  UVSTInstrument in 'VST3SDK\UVSTInstrument.pas',
  Vst3Base in 'VST3SDK\Vst3Base.pas',
  UCPlugView in 'VST3SDK\UCPlugView.pas',
  UAthenaVst in 'UAthenaVst.pas',
  UAthenaVstDSP in 'UAthenaVstDSP.pas',
  UAthenaVSTForm in 'UAthenaVSTForm.pas' {Form1},
  UPianoKeyboard in 'PianoComponent_Install\UPianoKeyboard.pas';

{$R *.res}

begin
end.
