
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fGettingStarted;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ComCtrls, Buttons, ExtCtrls, StdCtrls, Grids, DBGrids, DBCtrls, Mask,
  FireDAC.Stan.Intf, FireDAC.DatS, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI, FireDAC.Stan.Consts,
  FireDAC.VCLUI.Controls, FireDAC.Stan.Error, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Phys, FireDAC.Phys.ODBCBase, FireDAC.Phys.MSAcc, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.VCLUI.Wait, FireDAC.VCLUI.Error, FireDAC.VCLUI.Login;

type
  TfrmGettingStarted = class(TForm)
    pnlButtons: TPanel;
    pnlTitle: TPanel;
    pnlMain: TPanel;
    lblTitle: TLabel;
    imgAnyDAC: TImage;
    lblInfo: TLabel;
    imgInfo: TImage;
    bvlTitle: TBevel;
    bvlButtons: TBevel;
    qryProducts: TFDQuery;
    dsProducts: TDataSource;
    grdCategories: TDBGrid;
    pnlSubPageControl: TPanel;
    dlgOpen: TOpenDialog;
    accService: TFDMSAccessService;
    FDPhysMSAccessDriverLink1: TFDPhysMSAccessDriverLink;
    sbMain: TStatusBar;
    cbConnDefs: TComboBox;
    imgConnect: TImage;
    imgService: TImage;
    btnCompact: TButton;
    chbResetPassword: TCheckBox;
    edtPassword: TMaskEdit;
    Label1: TLabel;
    FDGUIxLoginDialog1: TFDGUIxLoginDialog;
    dbMain: TFDConnection;
    FDGUIxErrorDialog1: TFDGUIxErrorDialog;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    pnlConnDef: TPanel;
    Label2: TLabel;
    qryCategories: TFDQuery;
    Splitter1: TSplitter;
    grdProducts: TDBGrid;
    dsCategories: TDataSource;
    nvgCategories: TDBNavigator;
    pcMain: TPageControl;
    tsMD: TTabSheet;
    tsService: TTabSheet;
    btnCreateDB: TButton;
    Label3: TLabel;
    edtDatabase: TEdit;
    btnOpen: TSpeedButton;
    nvgProducts: TDBNavigator;
    pnlMisc: TPanel;
    btnUpdate: TButton;
    btnInsert: TButton;
    btnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbConnDefsClick(Sender: TObject);
    procedure btnCompactClick(Sender: TObject);
    procedure btnCreateDBClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure lblInfoClick(Sender: TObject);
    procedure imgAnyDACClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  end;

var
  frmGettingStarted: TfrmGettingStarted;

implementation

uses
  FireDAC.Stan.Util;

{$R *.dfm}

procedure TfrmGettingStarted.FormCreate(Sender: TObject);
begin
  cbConnDefs.Clear;
  cbConnDefs.Items.Add('<Open database...>');

  dlgOpen.InitialDir := FDManager.ActualConnectionDefFileName;
end;

procedure TfrmGettingStarted.cbConnDefsClick(Sender: TObject);
begin
  if not dlgOpen.Execute then
    Exit;
  with dbMain do begin
    Close;
    // create temporary connection definition
    with Params do begin
      Clear;
      Add('DriverID=MSAcc');
      Add('Database=' + dlgOpen.FileName);
    end;
    Open;
    qryCategories.Open;
    qryProducts.Open;
  end;
end;

procedure TfrmGettingStarted.btnInsertClick(Sender: TObject);
var
  iID: Integer;
begin
  if not dbMain.Connected then
    Exit;
  // Insert a record
  dbMain.ExecSQL('insert into Categories(CategoryName, Description, Picture) ' +
                 'values(:N, :D, :P)', ['New category', 'New description', $0334]);
  qryCategories.Refresh;

  // Get a scalar value from DB
  iID := dbMain.ExecSQLScalar('select MAX(CategoryID) from Categories');
  sbMain.SimpleText := 'Last CategoryID = ' + IntToStr(iID);
end;

procedure TfrmGettingStarted.btnUpdateClick(Sender: TObject);
begin
  if not dbMain.Connected then
    Exit;
  // Update records
  dbMain.ExecSQL('update Products set UnitPrice = UnitPrice * :P1 + :P2 ' +
                 'where ProductID < 3', [Random(5), Random(3)]);
  qryProducts.Refresh;
end;

procedure TfrmGettingStarted.btnDeleteClick(Sender: TObject);
begin
  if not dbMain.Connected then
    Exit;
  // Delete a record
  dbMain.ExecSQL('delete from Categories where CategoryName like :N',
    ['New category']);
  qryCategories.Refresh;
end;

procedure TfrmGettingStarted.btnCompactClick(Sender: TObject);
var
  sDb: String;
begin
  sDb := edtDatabase.Text;
  if sDb = '' then
    Exit;
  with accService do begin
    ResetPassword := chbResetPassword.Checked;
    // always specify database password if you're going to reset that
    Password := edtPassword.Text;
    Database := sDb;
    // setting/resetting password is done on compacting database
    Compact;
    if chbResetPassword.Checked then
      edtPassword.Text := '';
  end;
end;

procedure TfrmGettingStarted.btnCreateDBClick(Sender: TObject);
var
  sDb: String;
begin
  sDb := edtDatabase.Text;
  if sDb = '' then
    Exit;
  if not FileExists(sDb) then
    with accService do begin
      // if Password is not empty the created database will be password protected
      Password := edtPassword.Text;
      Database := sDb;
      CreateDB;
    end
  else
    ShowMessage('Database already exists!');
end;

procedure TfrmGettingStarted.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtDatabase.Text := dlgOpen.FileName;
end;

procedure TfrmGettingStarted.lblInfoClick(Sender: TObject);
var
  sHtmFile: String;
begin
  sHtmFile := ChangeFileExt(Application.ExeName, '.htm');
  FDShell(sHtmFile, '');
end;

procedure TfrmGettingStarted.imgAnyDACClick(Sender: TObject);
begin
  FDShell('http://www.embarcadero.com/products/rad-studio/firedac', '');
end;

end.
