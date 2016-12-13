//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formMergeMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ToolWin,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList, Vcl.StdActns,
  Vcl.Menus;

type
  TfrmMergeMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    Splitter1: TSplitter;
    FileOpenDialog1: TFileOpenDialog;
    edtMSSQLDatabase: TEdit;
    Panel3: TPanel;
    btnMerge: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    ProgressBar4: TProgressBar;
    ProgressBar5: TProgressBar;
    ProgressBar6: TProgressBar;
    Panel4: TPanel;
    GroupBox1: TGroupBox;
    sbInterBase: TSpeedButton;
    edtIBDatabase: TEdit;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    Exit1: TMenuItem;
    File1: TMenuItem;
    edtMSSQLServer: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Controller : TComponent;
  public
    { Public declarations }
  end;

var
  frmMergeMain: TfrmMergeMain;

implementation

{$R *.dfm}

uses formMergeMainController;


procedure TfrmMergeMain.FormCreate(Sender: TObject);
begin
  inherited;
  Controller := TMergeMainController.Create(Self, Self);
end;

procedure TfrmMergeMain.FormDestroy(Sender: TObject);
begin
  Controller.Free;
end;

end.
