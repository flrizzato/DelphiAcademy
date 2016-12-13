unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls,
  FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageXML,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.UI;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    Button1: TButton;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    FDMemTable2: TFDMemTable;
    DataSource2: TDataSource;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    FileOpenDialog1: TFileOpenDialog;
    FileSaveDialog1: TFileSaveDialog;
    FDMemTable3: TFDMemTable;
    DataSource3: TDataSource;
    FDMemTable4: TFDMemTable;
    DataSource4: TDataSource;
    DBGrid3: TDBGrid;
    DBGrid4: TDBGrid;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Label1: TLabel;
    TabSheet4: TTabSheet;
    FDMemTable5: TFDMemTable;
    DataSource5: TDataSource;
    FDMemTable6: TFDMemTable;
    DataSource6: TDataSource;
    DBGrid5: TDBGrid;
    DBGrid6: TDBGrid;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    TabSheet5: TTabSheet;
    Button14: TButton;
    DBGrid7: TDBGrid;
    Button15: TButton;
    FDMemTable7: TFDMemTable;
    DataSource7: TDataSource;
    Button16: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure FDMemTable4AfterPost(DataSet: TDataSet);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
  private
    { Private declarations }
    iCount: integer;
    fMemStream: TMemoryStream;
    procedure CreateAndFill(aDataSet: TFDMemTable; aRecs: integer);
    procedure CreateAndFillTurbo(aDataSet: TFDMemTable; aRecs: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
begin
  FDMemTable4.Close;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    FDMemTable5.LoadFromFile(FileOpenDialog1.FileName, sfAuto);
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  FDMemTable6.CloneCursor(FDMemTable5, True, True);
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  FDMemTable6.FilterChanges := [rtInserted, rtModified, rtDeleted];
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
  CreateAndFill(FDMemTable7, 1000);
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
  CreateAndFillTurbo(FDMemTable7, 1000);
end;

procedure TForm1.Button16Click(Sender: TObject);
begin
  FDMemTable7.Close;
  FDMemTable7.FieldDefs.Clear;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  with FDMemTable1.FieldDefs do
  begin
    with AddFieldDef do
    begin
      Name := 'f1';
      DataType := ftInteger;
    end;
    with AddFieldDef do
    begin
      Name := 'f2';
      DataType := ftString;
      Size := 50;
    end;
  end;

  FDMemTable1.Open;
  for i := 1 to 1000 do
    with FDMemTable1 do
    begin
      Append;
      Fields[0].AsInteger := i;
      Fields[1].AsString := 'Record ' + Format('%.*d', [4, i]);
      Post;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    FDMemTable2.LoadFromFile(FileOpenDialog1.FileName, sfAuto);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if FileSaveDialog1.Execute then
    FDMemTable2.SaveToFile(FileSaveDialog1.FileName, sfAuto);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  fMemStream := TMemoryStream.Create;
  FDMemTable2.SaveToStream(fMemStream, sfBinary);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if fMemStream <> nil then
  begin
    fMemStream.Position := 0;
    FDMemTable2.LoadFromStream(fMemStream, sfBinary);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FDMemTable2.Close;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  iCount := 0;
  Label1.Caption := 'Rec #0000';
  FDMemTable4.CopyDataSet(FDMemTable3, [coStructure, coRestart, coAppend]);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  iCount := 0;
  Label1.Caption := 'Rec #0000';
  FDMemTable4.Data := FDMemTable3.Data;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  CreateAndFill(FDMemTable3, 1000);
end;

procedure TForm1.CreateAndFill(aDataSet: TFDMemTable; aRecs: integer);
var
  i: integer;
begin
  with aDataSet.FieldDefs do
  begin
    with AddFieldDef do
    begin
      Name := 'f1';
      DataType := ftInteger;
    end;
    with AddFieldDef do
    begin
      Name := 'f2';
      DataType := ftString;
      Size := 15;
    end;
    with AddFieldDef do
    begin
      Name := 'f3';
      DataType := ftDateTime;
    end;
  end;

  aDataSet.Open;
  for i := 1 to aRecs do
    with aDataSet do
    begin
      Append;
      Fields[0].AsInteger := i;
      Fields[1].AsString := 'Record ' + Format('%.*d', [4, i]);
      Fields[2].AsDateTime := Now;
      Post;
    end;
end;

procedure TForm1.CreateAndFillTurbo(aDataSet: TFDMemTable; aRecs: integer);
var
  i: integer;
begin
  with aDataSet.FieldDefs do
  begin
    with AddFieldDef do
    begin
      Name := 'f1';
      DataType := ftInteger;
    end;
    with AddFieldDef do
    begin
      Name := 'f2';
      DataType := ftString;
      Size := 15;
    end;
    with AddFieldDef do
    begin
      Name := 'f3';
      DataType := ftDateTime;
    end;
  end;

  aDataSet.Open;
  aDataSet.LogChanges := False;
  aDataSet.FetchOptions.RecsMax := aRecs;
  aDataSet.ResourceOptions.SilentMode := True;
  aDataSet.UpdateOptions.LockMode := lmNone;
  aDataSet.UpdateOptions.LockPoint := lpDeferred;
  aDataSet.UpdateOptions.FetchGeneratorsPoint := gpImmediate;

  aDataSet.BeginBatch;
  try
    for i := 1 to aRecs do
      with aDataSet do
      begin
        Append;
        Fields[0].AsInteger := i;
        Fields[1].AsString := 'Record ' + Format('%.*d', [4, i]);
        Fields[2].AsDateTime := Now;
        Post;
      end;
  finally
    aDataSet.EndBatch;
  end;
end;

procedure TForm1.FDMemTable4AfterPost(DataSet: TDataSet);
begin
  Inc(iCount);
  Label1.Caption := 'Rec #' + iCount.ToString;
  Application.ProcessMessages;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fMemStream := nil;
end;

end.
