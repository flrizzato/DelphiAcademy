unit Brparts;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, DB, DBCtrls, StdCtrls, ExtCtrls, Grids, DBGrids, Buttons;

type
  TBrPartsForm = class(TForm)
    Panel1: TPanel;
    Navigator: TDBNavigator;
    ActivateBtn: TSpeedButton;
    EditBtn: TButton;
    Panel3: TPanel;
    CloseBtn: TButton;
    PartsGrid: TDBGrid;
    Bevel1: TBevel;
    procedure ActivateQuery(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetPartNo: Double;
    procedure SetPartNo(NewPartNo: Double);
  public
    property PartNo: Double read GetPartNo write SetPartNo;
  end;

var
  BrPartsForm: TBrPartsForm;

implementation

uses EdParts, DataMod;

{$R *.dfm}

function TBrPartsForm.GetPartNo: Double;
begin
  Result := MastData.PartsSource.Dataset.Fields[0].AsFloat;
end;

procedure TBrPartsForm.SetPartNo(NewPartNo: Double);
begin
  MastData.PartsSource.Dataset := MastData.Parts;
  MastData.Parts.Locate('PartNo', NewPartNo, []);
end;          

procedure TBrPartsForm.ActivateQuery(Sender: TObject);
begin
  if not ActivateBtn.Down then
    MastData.PartsSource.Dataset := MastData.Parts
  else
    try
      MastData.PartsQuery.Close;
      MastData.PartsQuery.Open;
      MastData.PartsSource.Dataset := MastData.PartsQuery;
    except
      MastData.PartsSource.Dataset := MastData.Parts;
      raise;
    end;
end;

procedure TBrPartsForm.EditBtnClick(Sender: TObject);
begin
  if ActivateBtn.Down then
  begin
    EdPartsForm.Edit(MastData.PartsQueryPartNo.Value);
    MastData.PartsQuery.Close;
    MastData.PartsQuery.Open;
  end else
    EdPartsForm.Edit(MastData.PartsPartNo.Value);
end;

procedure TBrPartsForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TBrPartsForm.FormShow(Sender: TObject);
begin
  MastData.Parts.Open;
end;

end.
