unit uAbstractDataTabForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataForm, DB, DBActns, ActnList, ImgList, ComCtrls,
  ToolWin, Grids, DBGrids, StdCtrls, Buttons, ExtCtrls, System.Actions,
  System.ImageList;

type
  TAbstractDataTabForm = class(TAbstractDataForm)
    PageControl: TPageControl;
    tabEdit: TTabSheet;
    tabSearch: TTabSheet;
    panBotton: TPanel;
    lblDataFind: TLabel;
    lblField: TLabel;
    lblValue: TLabel;
    cmbField: TComboBox;
    edtValue: TEdit;
    btnSearch: TBitBtn;
    DBGridSearch: TDBGrid;
    procedure PageControlChange(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
    aFieldNames: array of string;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAbstractDataTabForm.btnSearchClick(Sender: TObject);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  datControl.DataSet.DisableControls;
  try
    if (cmbField.ItemIndex >= 0) and (edtValue.Text <> '') then
      datControl.DataSet.Locate(aFieldNames[cmbField.ItemIndex], edtValue.Text,
        [loCaseInsensitive, loPartialKey]);
  finally
    datControl.DataSet.EnableControls;
    Screen.Cursor := crDefault;
  end;
end;

procedure TAbstractDataTabForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  inherited;
  for i := 0 to datControl.DataSet.Fields.Count - 1 do
    if (datControl.DataSet.Fields[i].FieldKind = fkData) and
      (datControl.DataSet.Fields[i].Tag <> -2) then
    begin
      cmbField.Items.Add(datControl.DataSet.Fields[i].DisplayLabel);
      SetLength(aFieldNames, Length(aFieldNames) + 1);
      aFieldNames[Length(aFieldNames) - 1] := datControl.DataSet.Fields[i]
        .FieldName;
    end;
  cmbField.ItemIndex := 0;
end;

procedure TAbstractDataTabForm.PageControlChange(Sender: TObject);
begin
  inherited;
  DataSetInsert1.Visible := PageControl.ActivePageIndex = 0;
  DataSetDelete1.Visible := PageControl.ActivePageIndex = 0;
  DataSetEdit1.Visible := PageControl.ActivePageIndex = 0;
end;

procedure TAbstractDataTabForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange := not(datControl.DataSet.State in [dsEdit, dsInsert]);
end;

end.
