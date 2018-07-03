unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, CustomizeDlg, XPStyleActnCtrls, ActnList,
  ActnMan, ImgList, Menus, StdActns, ToolWin, ActnCtrls, BandActn,
  uAbstractForm, System.Actions, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  FireDAC.DBX.Migrate, System.ImageList;

type
  TMainForm = class(TAbstractForm)
    MainMenu: TMainMenu;
    ImageList: TImageList;
    ActionManager: TActionManager;
    CustomizeDlg: TCustomizeDlg;
    Timer: TTimer;
    StatusBar: TStatusBar;
    mnuFile: TMenuItem;
    mnuControl: TMenuItem;
    mnuTools: TMenuItem;
    mnuWindow: TMenuItem;
    mnuHelp: TMenuItem;
    ActionToolBar: TActionToolBar;
    WindowClose1: TWindowClose;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    Arrange1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    MinimizeAll1: TMenuItem;
    Arrange2: TMenuItem;
    Close1: TMenuItem;
    CustomizeActionBars1: TCustomizeActionBars;
    mnuCustomize: TMenuItem;
    mnuUser: TMenuItem;
    mnuRoom: TMenuItem;
    mnuClose: TMenuItem;
    N1: TMenuItem;
    mnuMeetingManager: TMenuItem;
    actClose: TAction;
    acUser: TAction;
    acRoom: TAction;
    acMeetingManager: TAction;
    HelpContents1: THelpContents;
    HelpTopicSearch1: THelpTopicSearch;
    HelpOnHelp1: THelpOnHelp;
    HelpContextAction1: THelpContextAction;
    Contents1: TMenuItem;
    HelpContextAction11: TMenuItem;
    opicSearch1: TMenuItem;
    procedure acMeetingManagerExecute(Sender: TObject);
    procedure acRoomExecute(Sender: TObject);
    procedure acUserExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    fDBConnection: TFDConnection;
    procedure SetDBConnection(value: TFDConnection);
    function GetDBConnection: TFDConnection;
  private
    { Private declarations }
  public
    property DBConnection: TFDConnection read GetDBConnection
      write SetDBConnection;
  end;

var
  MainForm: TMainForm;

implementation

uses
  uUserControl, uRoomControl, uMeetingControl;

{$R *.dfm}

procedure TMainForm.acUserExecute(Sender: TObject);
begin
  inherited;
  TUserControl.GetInstance.CreateDefaultForm;
end;

procedure TMainForm.acRoomExecute(Sender: TObject);
begin
  inherited;
  TRoomControl.GetInstance.CreateDefaultForm;
end;

procedure TMainForm.acMeetingManagerExecute(Sender: TObject);
begin
  inherited;
  TMeetingControl.GetInstance.CreateDefaultForm;
end;

procedure TMainForm.actCloseExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  inherited;
  StatusBar.Panels[2].Text := 'Welcome [' +
    TUserControl.GetInstance.fUserName + ']';
  StatusBar.Panels[3].Text := 'Is Admin: ';

  acUser.Enabled := TUserControl.GetInstance.fUserIsAdmin;
  acRoom.Enabled := TUserControl.GetInstance.fUserIsAdmin;

  if TUserControl.GetInstance.fUserIsAdmin then
    StatusBar.Panels[3].Text := StatusBar.Panels[3].Text + 'YES'
  else
    StatusBar.Panels[3].Text := StatusBar.Panels[3].Text + 'NO';
end;

function TMainForm.GetDBConnection: TFDConnection;
begin
  result := fDBConnection;
end;

procedure TMainForm.SetDBConnection(value: TFDConnection);
begin
  fDBConnection := value;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'Date: ' + DateToStr(Date);
  StatusBar.Panels[1].Text := 'Time: ' + TimeToStr(Time);
end;

end.
