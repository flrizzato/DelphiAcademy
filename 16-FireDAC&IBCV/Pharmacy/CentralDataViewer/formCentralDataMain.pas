//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formCentralDataMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Menus, Vcl.ExtCtrls,
  Vcl.Grids, Vcl.DBGrids, dmDatabase, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.DBActns,
  System.Actions, Vcl.ActnList;

type
  TForm2 = class(TForm)
    DBGrid2: TDBGrid;
    Splitter1: TSplitter;
    MainMenu1: TMainMenu;
    Data1: TMenuItem;
    Refresh1: TMenuItem;
    dsCategory: TDataSource;
    dsMedicine: TDataSource;
    Label1: TLabel;
    DBGrid3: TDBGrid;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    Panel2: TPanel;
    dsLinks: TDataSource;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    Panel3: TPanel;
    Panel4: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    ActionList1: TActionList;
    DatasetInsert1: TDataSetInsert;
    DatasetDelete1: TDataSetDelete;
    Splitter2: TSplitter;
    procedure Refresh1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    DM: TdtmdDatabase;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  DM := TdtmdDatabase.Create(Self);
  Refresh1.Click;
  dsCategory.DataSet := DM.qryCategory;
  dsMedicine.DataSet := DM.qryMedicine;
  dsLinks.DataSet := DM.qryMedicineCategories;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  DM.Free;
end;

procedure TForm2.Refresh1Click(Sender: TObject);
begin
  DM.FDConnection.Close;
  DM.FDConnection.Open;
end;

end.
