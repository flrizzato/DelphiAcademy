//---------------------------------------------------------------------------

// This software is Copyright (c) 2017 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uCardPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.WinXPanels, Vcl.StdCtrls, System.Actions, Vcl.ActnList,
  Vcl.Touch.GestureMgr, Vcl.Imaging.jpeg;

type
  TCardPanelForm = class(TForm)
    pnlToolbar: TPanel;
    CardPanel1: TCardPanel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    actNextCard: TAction;
    actPreviousCard: TAction;
    chkLoop: TCheckBox;
    Card1: TCard;
    Card2: TCard;
    Card3: TCard;
    Button1: TButton;
    Button2: TButton;
    lblGestureInfo: TLabel;
    lblVclStyle: TLabel;
    cbxVclStyles: TComboBox;
    Card4: TCard;
    Card5: TCard;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Card6: TCard;
    Card7: TCard;
    procedure FormCreate(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
    procedure actNextCardExecute(Sender: TObject);
    procedure actPreviousCardExecute(Sender: TObject);
    procedure chkLoopClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CardPanelForm: TCardPanelForm;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

procedure TCardPanelForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  CardPanel1.ActiveCardIndex := 0;

  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TCardPanelForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TCardPanelForm.chkLoopClick(Sender: TObject);
begin
  CardPanel1.Loop := chkLoop.Checked;
end;

procedure TCardPanelForm.actNextCardExecute(Sender: TObject);
begin
  CardPanel1.NextCard;
end;

procedure TCardPanelForm.actPreviousCardExecute(Sender: TObject);
begin
  CardPanel1.PreviousCard;
end;

end.
