unit uFormJsonViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, JSONTreeView, JSONDoc, Vcl.Menus,
  System.Actions, Vcl.ActnList, Vcl.StdCtrls;

type
  TFormJsonView = class(TForm)
    ImageListMain: TImageList;
    ToolBar1: TToolBar;
    ToolButtonURL: TToolButton;
    ToolButtonFile: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonClear: TToolButton;
    StatusBarMain: TStatusBar;
    JSONDocMain: TJSONDocument;
    JSONTreeViewMain: TJSONTreeView;
    PopupMenuJsonTreeView: TPopupMenu;
    MenuItem_VisibleChildrenCounts: TMenuItem;
    MenuItem_VisibleByteSizes: TMenuItem;
    ActionListMain: TActionList;
    ActionLoadFromURL: TAction;
    ActionLoadFromFile: TAction;
    ActionPasteFromClipboard: TAction;
    ActionClear: TAction;
    ActionToggleVisibleCounts: TAction;
    ActionToggleVisibleBytes: TAction;
    FileOpenDialog1: TFileOpenDialog;
    edtURL: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ActionLoadFromURLExecute(Sender: TObject);
    procedure ActionLoadFromFileExecute(Sender: TObject);
    procedure ActionPasteFromClipboardExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionToggleVisibleCountsExecute(Sender: TObject);
    procedure ActionToggleVisibleBytesExecute(Sender: TObject);
  private
    procedure ShowJson(s: string);
  public
    { Public declarations }
  end;

var
  FormJsonView: TFormJsonView;

implementation

uses
  System.IOUtils, Vcl.Clipbrd, uDMHTTP;

{$R *.dfm}

procedure TFormJsonView.FormCreate(Sender: TObject);
begin
  ActionToggleVisibleCounts.Checked := JSONTreeViewMain.VisibleChildrenCounts;
  ActionToggleVisibleBytes.Checked := JSONTreeViewMain.VisibleByteSizes;
end;

procedure TFormJsonView.ShowJson(s: string);
begin
  JSONDocMain.JsonText := s;

  JSONTreeViewMain.Items.BeginUpdate;
  try
    JSONTreeViewMain.LoadJson;
    JSONTreeViewMain.FullCollapse;
  finally
    JSONTreeViewMain.Items.EndUpdate;
  end;
end;

procedure TFormJsonView.ActionClearExecute(Sender: TObject);
begin
  JSONDocMain.JsonText := '';
  JSONTreeViewMain.LoadJson;
end;

procedure TFormJsonView.ActionLoadFromFileExecute(Sender: TObject);
var jsonStr: string;
begin
  if FileOpenDialog1.Execute then
  begin
    jsonStr := TFile.ReadAllText(FileOpenDialog1.FileName);
    ShowJson(jsonStr);
  end;
end;

procedure TFormJsonView.ActionLoadFromURLExecute(Sender: TObject);
var jsonStr: string;
begin
  if edtURL.Text <> '' then
  begin
    ActionClear.Execute;
    ActionLoadFromURL.Enabled := False;
    StatusBarMain.SimpleText := 'Loading JSON... Please Wait...';
    Application.ProcessMessages;

    if DMHTTP = nil then
      DMHTTP := TDMHTTP.Create(Application);

    jsonStr := DMHTTP.GetRespString(edtURL.Text);
    ShowJson(jsonStr);

    StatusBarMain.SimpleText := '';
    ActionLoadFromURL.Enabled := True;
  end
  else
  begin
    ShowMessage('Enter URL to load JSON');
    edtURL.SetFocus;
  end;
end;

procedure TFormJsonView.ActionPasteFromClipboardExecute(Sender: TObject);
begin
  ShowJson(Clipboard.AsText);
end;

procedure TFormJsonView.ActionToggleVisibleBytesExecute(Sender: TObject);
begin
  JSONTreeViewMain.VisibleByteSizes := not JSONTreeViewMain.VisibleByteSizes;
end;

procedure TFormJsonView.ActionToggleVisibleCountsExecute(Sender: TObject);
begin
  JSONTreeViewMain.VisibleChildrenCounts := not JSONTreeViewMain.VisibleChildrenCounts;
end;

end.
