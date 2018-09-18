program MeetingOrganizer;

uses
  Vcl.Forms,
  uAbstractControl in 'uAbstractControl.pas',
  uAbstractForm in 'uAbstractForm.pas' {AbstractForm} ,
  uAbstractDataModule
    in 'uAbstractDataModule.pas' {AbstractDataModule: TDataModule} ,
  uAbstractDataForm in 'uAbstractDataForm.pas' {AbstractDataForm} ,
  uAbstractDataTabForm in 'uAbstractDataTabForm.pas' {AbstractDataTabForm} ,
  uMainDM in 'uMainDM.pas' {MainDM: TDataModule} ,
  uMainForm in 'uMainForm.pas' {MainForm} ,
  uMeetingControl in 'uMeetingControl.pas',
  uMeetingDM in 'uMeetingDM.pas' {MeetingDM: TDataModule} ,
  uMeetingForm in 'uMeetingForm.pas' {MeetingForm} ,
  uMeetingTimeRoomForm in 'uMeetingTimeRoomForm.pas' {MeetingTimeRoomForm} ,
  uMsgControl in 'uMsgControl.pas',
  uRecError in 'uRecError.pas' {ReconcileErrorForm} ,
  uRoomControl in 'uRoomControl.pas',
  uRoomDM in 'uRoomDM.pas' {RoomDM: TDataModule} ,
  uRoomForm in 'uRoomForm.pas' {RoomForm} ,
  uUserControl in 'uUserControl.pas',
  uUserDM in 'uUserDM.pas' {UserDM: TDataModule} ,
  uUserForm in 'uUserForm.pas' {UserForm} ,
  uUserLoginForm in 'uUserLoginForm.pas' {UserLoginForm} ,
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := '';
  TStyleManager.TrySetStyle('Sapphire Kamri');
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainDM, MainDM);
  if TUserControl.GetInstance.DoUserLogin then
    Application.Run
  else
    Application.Terminate;

end.
