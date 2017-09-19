unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, DKLang,
  FMX.StdCtrls, FMX.ListBox, DKLangStorage, FMX.Layouts, FMX.Memo,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TfMain = class(TForm)
    lcMain: TDKLanguageController;
    cbLanguage: TComboBox;
    lSampleMessage: TLabel;
    bTest: TButton;
    tsMain: TDKLTranslationsStorage;
    pMsg: TPanel;
    lMessageCaption: TLabel;
    mMessage: TMemo;
    procedure bTestClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.fmx}

procedure TfMain.bTestClick(Sender: TObject);
begin
  // simulate a dialog
  lMessageCaption.Text := DKLangConstW('SMessageCaption');
  mMessage.Lines.Text := DKLangConstW('STestMessage');
end;

procedure TfMain.cbLanguageChange(Sender: TObject);
var iIndex: Integer;
begin
  iIndex := cbLanguage.ItemIndex;
  if iIndex<0 then iIndex := 0; // When there's no valid selection in cbLanguage we use the default language (Index=0)
  LangManager.LanguageID := LangManager.LanguageIDs[iIndex];
end;

procedure TfMain.FormCreate(Sender: TObject);
  var i: Integer;
  begin
     // Fill cbLanguage with available languages
    for i := 0 to LangManager.LanguageCount-1 do cbLanguage.Items.Add(LangManager.LanguageNames[i]);
     // Index=0 always means the default language
    cbLanguage.ItemIndex := 0;
  end;

end.
