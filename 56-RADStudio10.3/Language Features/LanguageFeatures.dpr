program LanguageFeatures;

uses
  System.StartUpCopy,
  FMX.Forms,
  formInlineVariables1 in 'formInlineVariables1.pas' {frmInlineVariables1},
  formInlineVariables2 in 'formInlineVariables2.pas' {frmInlineVariables2},
  formInlineVariables3 in 'formInlineVariables3.pas' {frmInlineVariables3},
  formTypeInference2 in 'formTypeInference2.pas' {frmTypeInference2},
  formTypeInference1 in 'formTypeInference1.pas' {frmTypeInference1},
  formInlineConstants in 'formInlineConstants.pas' {frmInlineConstants},
  formLinuxAnsiStrings in 'formLinuxAnsiStrings.pas' {frmLinuxAnsiStrings},
  formCppCompatibiliy in 'formCppCompatibiliy.pas' {frmCppCompatibility};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmInlineVariables1, frmInlineVariables1);
  Application.CreateForm(TfrmInlineVariables2, frmInlineVariables2);
  Application.CreateForm(TfrmInlineVariables3, frmInlineVariables3);
  Application.CreateForm(TfrmTypeInference2, frmTypeInference2);
  Application.CreateForm(TfrmTypeInference1, frmTypeInference1);
  Application.CreateForm(TfrmInlineConstants, frmInlineConstants);
  Application.CreateForm(TfrmLinuxAnsiStrings, frmLinuxAnsiStrings);
  Application.CreateForm(TfrmCppCompatibility, frmCppCompatibility);
  Application.Run;
end.
