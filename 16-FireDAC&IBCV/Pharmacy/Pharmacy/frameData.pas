//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit frameData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls,
  Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls;

type
  TFrame1 = class(TFrame)
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    lblQry: TLabel;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Splitter2: TSplitter;
    dsCurrent: TDataSource;
    dsDelta: TDataSource;
    procedure DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrame1.DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect;
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
      TDBGrid(Sender).Canvas.Brush.Color := clLime;
    usDeleted:
      TDBGrid(Sender).Canvas.Brush.Color := clRed;
    end;
    TDBGrid(Sender).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TFrame1.FrameResize(Sender: TObject);
begin
  GroupBox3.Height := Trunc((Self.Height-Splitter2.Height) / 2);
end;

end.
