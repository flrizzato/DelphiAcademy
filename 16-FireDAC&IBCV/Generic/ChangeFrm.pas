//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ChangeFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Phys.IBBase, FireDAC.Comp.UI,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.Grids,
  Vcl.DBGrids, FireDAC.Stan.StorageXML;

type
  TfrmChange = class(TForm)
    conOriginal: TFDConnection;
    qChanges: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    dsChanges: TDataSource;
    grdChanges: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    grdRemote: TDBGrid;
    mtRemote: TFDMemTable;
    dsRemote: TDataSource;
    btnMergeData: TButton;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    btnCommitUpd: TButton;
    grdOriginal: TDBGrid;
    qOriginal: TFDQuery;
    dsOriginal: TDataSource;
    conChanges: TFDConnection;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    btnOpenDB: TButton;
    btnMergeDelta: TButton;
    btnUndoLast: TButton;
    eaChanges: TFDEventAlerter;
    Label15: TLabel;
    cbChangesData: TComboBox;
    Label16: TLabel;
    cbChangesRefresh: TComboBox;
    cbxUseStream: TCheckBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    procedure dsChangesDataChange(Sender: TObject; Field: TField);
    procedure grdChangesColEnter(Sender: TObject);
    procedure btnMergeDataClick(Sender: TObject);
    procedure btnCommitUpdClick(Sender: TObject);
    procedure dsRemoteDataChange(Sender: TObject; Field: TField);
    procedure grdRemoteColEnter(Sender: TObject);
    procedure btnOpenDBClick(Sender: TObject);
    procedure btnMergeDeltaClick(Sender: TObject);
    procedure btnUndoLastClick(Sender: TObject);
    procedure grdRemoteDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure cbChangesDataChange(Sender: TObject);
    procedure cbChangesRefreshChange(Sender: TObject);
  private
    procedure DisplayStatus(ALabSt, ALabOld, ALabCur, ALabNew: TLabel;
      ADS: TFDDataSet; AFld: TField);
  public
    { Public declarations }
  end;

var
  frmChange: TfrmChange;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------}
{ GUI event handlers / setup                                                    }
{-------------------------------------------------------------------------------}
procedure TfrmChange.cbChangesDataChange(Sender: TObject);
var
  eData: TFDMergeDataMode;
begin
  case cbChangesData.ItemIndex of
  0:   eData := dmDataMerge;
  1:   eData := dmDeltaMerge;
  2:   eData := dmDeltaSet;
  else eData := dmDataMerge;
  end;
  eaChanges.Options.MergeData := eData;
end;

procedure TfrmChange.cbChangesRefreshChange(Sender: TObject);
var
  eRefresh: TFDAutoRefresh;
begin
  case cbChangesRefresh.ItemIndex of
  0:   eRefresh := afNone;
  1:   eRefresh := afAlert;
  2:   eRefresh := afTimeout;
  else eRefresh := afAlert;
  end;
  eaChanges.Options.AutoRefresh := eRefresh;
  if eaChanges.Options.AutoRefresh = afTimeout then
    eaChanges.Options.Timeout := 3000
  else
    eaChanges.Options.Timeout := -1;
end;

{-------------------------------------------------------------------------------}
{ GUI event handlers / drawing                                                  }
{-------------------------------------------------------------------------------}
procedure TfrmChange.DisplayStatus(ALabSt, ALabOld, ALabCur, ALabNew: TLabel;
  ADS: TFDDataSet; AFld: TField);
var
  oRow: TFDDatSRow;
  s: String;
begin
  case ADS.UpdateStatus of
  usUnmodified: s := '--';
  usModified:   s := 'Mod';
  usInserted:   s := 'Ins';
  usDeleted:    s := 'Del';
  end;
  oRow := ADS.GetRow();
  if oRow = nil then
    s := s + '/--'
  else
    s := s + '/' + IntToStr(oRow.RowID);
  ALabSt.Caption := s;
  if AFld = nil then begin
    ALabOld.Caption := '--';
    ALabCur.Caption := '--';
    ALabNew.Caption := '--';
  end
  else begin
    ALabOld.Caption := VarToStrDef(AFld.OldValue, 'null');
    ALabCur.Caption := VarToStrDef(AFld.CurValue, 'null');
    ALabNew.Caption := VarToStrDef(AFld.NewValue, 'null');
  end;
end;

procedure TfrmChange.dsChangesDataChange(Sender: TObject; Field: TField);
begin
  DisplayStatus(Label1, Label3, Label5, Label7, qChanges, grdChanges.SelectedField);
end;

procedure TfrmChange.grdChangesColEnter(Sender: TObject);
begin
  dsChangesDataChange(nil, nil);
end;

procedure TfrmChange.dsRemoteDataChange(Sender: TObject; Field: TField);
begin
  DisplayStatus(Label8, Label10, Label12, Label14, mtRemote, grdRemote.SelectedField);
end;

procedure TfrmChange.grdRemoteColEnter(Sender: TObject);
begin
  dsRemoteDataChange(nil, nil);
end;

procedure TfrmChange.grdRemoteDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if not (gdFocused in State) and (Column.Field <> nil) then begin
    case Column.Field.DataSet.UpdateStatus of
    usUnmodified:
      ;
    usModified:
      begin
        TDBGrid(Sender).Canvas.Brush.Color := clYellow;
        if VarCompareValue(Column.Field.OldValue, Column.Field.CurValue) <> vrEqual then
          TDBGrid(Sender).Canvas.Font.Style := [fsBold];
      end;
    usInserted:
      TDBGrid(Sender).Canvas.Brush.Color := clFuchsia;
    usDeleted:
      TDBGrid(Sender).Canvas.Brush.Color := clDkGray;
    end;
    TDBGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

{-------------------------------------------------------------------------------}
{ "Thick" client (2nd grid)                                                     }
{-------------------------------------------------------------------------------}
procedure TfrmChange.btnOpenDBClick(Sender: TObject);
begin
  conOriginal.Connected := True;
  qOriginal.Active := True;
  qOriginal.Fields[0].ProviderFlags := qOriginal.Fields[0].ProviderFlags + [pfInUpdate];
  qOriginal.UpdateOptions.GeneratorName := 'GEN';

  conChanges.Params := conOriginal.Params;
  conChanges.Connected := True;
  qChanges.Active := True;
  eaChanges.Active := True;
end;

{-------------------------------------------------------------------------------}
{ "Thin" client (3d grid)                                                       }
{-------------------------------------------------------------------------------}
procedure TfrmChange.btnMergeDataClick(Sender: TObject);
var
  oStr: TMemoryStream;
begin
  if cbxUseStream.Checked then begin
    oStr := TMemoryStream.Create;
    try
      qChanges.SaveToStream(oStr);
      mtRemote.ResourceOptions.StoreItems := [siData, siDelta, siMeta];
      mtRemote.ResourceOptions.StoreMergeData := dmDataMerge;
      mtRemote.ResourceOptions.StoreMergeMeta := mmAddOrError;
      oStr.Position := 0;
      mtRemote.LoadFromStream(oStr);
    finally
      oStr.Free;
    end;
  end
  else
    mtRemote.MergeDataSet(qChanges, dmDataMerge, mmUpdate);
end;

procedure TfrmChange.btnMergeDeltaClick(Sender: TObject);
var
  oStr: TMemoryStream;
begin
  if cbxUseStream.Checked then begin
    oStr := TMemoryStream.Create;
    try
      qChanges.SaveToStream(oStr);
      mtRemote.ResourceOptions.StoreItems := [siData, siDelta, siMeta];
      mtRemote.ResourceOptions.StoreMergeData := dmDeltaMerge;
      mtRemote.ResourceOptions.StoreMergeMeta := mmAddOrError;
      oStr.Position := 0;
      mtRemote.LoadFromStream(oStr);
    finally
      oStr.Free;
    end;
  end
  else
    mtRemote.MergeDataSet(qChanges, dmDeltaMerge, mmUpdate);
end;

procedure TfrmChange.btnCommitUpdClick(Sender: TObject);
begin
  mtRemote.CommitUpdates;
end;

procedure TfrmChange.btnUndoLastClick(Sender: TObject);
begin
  mtRemote.UndoLastChange(True);
end;

end.
