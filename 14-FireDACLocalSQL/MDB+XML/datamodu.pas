unit datamodu;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Datasnap.DBClient, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.SQLiteVDataSet,
  FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, FireDAC.Phys.IBBase, FireDAC.Phys.IB,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSAcc, Forms, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.MSAccDef;

type
  TMainDM = class(TDataModule)
    SQLLiteCnn: TFDConnection;
    FDLocalSQL1: TFDLocalSQL;
    OrdersQuery: TFDQuery;
    EmployeeCDS: TClientDataSet;
    FDQuery1: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    MSAccessCnn: TFDConnection;
    FDPhysMSAccessDriverLink1: TFDPhysMSAccessDriverLink;
    OrdersQueryOrderID: TFDAutoIncField;
    OrdersQueryCustomerID: TWideStringField;
    OrdersQueryEmployeeID: TIntegerField;
    OrdersQueryOrderDate: TSQLTimeStampField;
    OrdersQueryRequiredDate: TSQLTimeStampField;
    OrdersQueryShippedDate: TSQLTimeStampField;
    OrdersQueryShipVia: TIntegerField;
    OrdersQueryFreight: TCurrencyField;
    OrdersQueryShipName: TWideStringField;
    OrdersQueryShipAddress: TWideStringField;
    OrdersQueryShipCity: TWideStringField;
    OrdersQueryShipRegion: TWideStringField;
    OrdersQueryShipPostalCode: TWideStringField;
    OrdersQueryShipCountry: TWideStringField;
    EmployeeCDSEmpNo: TIntegerField;
    EmployeeCDSLastName: TStringField;
    EmployeeCDSFirstName: TStringField;
    EmployeeCDSPhoneExt: TStringField;
    EmployeeCDSHireDate: TDateTimeField;
    EmployeeCDSSalary: TFloatField;
    FDQuery1Employee: TWideStringField;
    FDQuery1OrderID: TFDAutoIncField;
    FDQuery1CustomerID: TWideStringField;
    FDQuery1EmployeeID: TIntegerField;
    FDQuery1OrderDate: TSQLTimeStampField;
    FDQuery1RequiredDate: TSQLTimeStampField;
    FDQuery1ShippedDate: TSQLTimeStampField;
    FDQuery1ShipVia: TIntegerField;
    FDQuery1Freight: TCurrencyField;
    FDQuery1ShipName: TWideStringField;
    FDQuery1ShipAddress: TWideStringField;
    FDQuery1ShipCity: TWideStringField;
    FDQuery1ShipRegion: TWideStringField;
    FDQuery1ShipPostalCode: TWideStringField;
    FDQuery1ShipCountry: TWideStringField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainDM: TMainDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
