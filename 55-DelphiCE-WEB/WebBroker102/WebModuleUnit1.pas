unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.ConsoleUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Web.DSProd, Web.DBWeb, Web.HTTPProd;

type
  TWebModule1 = class(TWebModule)
    EmployeeConnection: TFDConnection;
    EmployeeTable: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    pageHead: TPageProducer;
    pageFooter: TPageProducer;
    dsTableProcedure: TDataSetTableProducer;
    dsPageProducer: TDataSetPageProducer;
    EmployeeTableEMP_NO: TSmallintField;
    EmployeeTableFIRST_NAME: TStringField;
    EmployeeTableLAST_NAME: TStringField;
    EmployeeTablePHONE_EXT: TStringField;
    EmployeeTableHIRE_DATE: TSQLTimeStampField;
    EmployeeTableSALARY: TBCDField;
    procedure TimeAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure DateAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure MenuAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure StatusAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure RecordAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure TableAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure dsTableProcedureFormatCell(Sender: TObject; CellRow,
      CellColumn: Integer; var BgColor: THTMLBgColor; var Align: THTMLAlign;
      var VAlign: THTMLVAlign; var CustomAttrs, CellData: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule1.WebModuleAfterDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := pageHead.Content + Response.Content + pageFooter.Content;
end;

procedure TWebModule1.DateAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := 'Today is ' + FormatDateTime('dddd, mmmm d, yyyy',
    Now) + '<p>';
end;

procedure TWebModule1.TimeAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := 'Time at this site: ' + FormatDateTime('hh:mm:ss AM/PM',
    Now) + '<p>';
end;

procedure TWebModule1.StatusAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  I: Integer;
begin
  Response.Content := '<H3>Status</H3>'#13 +
    'Method: ' + Request.Method + '<br>'#13 +
    'ProtocolVersion: ' + Request.ProtocolVersion + '<br>'#13 +
    'URL: ' + Request.URL + '<br>'#13 +
    'Query: ' + Request.Query + '<br>'#13 +
    'PathInfo: ' + Request.PathInfo + '<br>'#13 +
    'PathTranslated: ' + Request.PathTranslated + '<br>'#13 +
    'Authorization: ' + Request.Authorization + '<br>'#13 +
    'CacheControl: ' + Request.CacheControl + '<br>'#13 +
    'Cookie: ' + Request.Cookie + '<br>'#13 +
    'Date: ' + DateTimeToStr (Request.Date) + '<br>'#13 +
    'Accept: ' + Request.Accept + '<br>'#13 +
    'From: ' + Request.From + '<br>'#13 +
    'Host: ' + Request.Host + '<br>'#13 +
    'IfModifiedSince: ' + DateTimeToStr (Request.IfModifiedSince) + '<br>'#13 +
    'Referer: ' + Request.Referer + '<br>'#13 +
    'UserAgent: ' + Request.UserAgent + '<br>'#13 +
    'ContentEncoding: ' + Request.ContentEncoding + '<br>'#13 +
    'ContentType: ' + Request.ContentType + '<br>'#13 +
    'ContentLength: ' + IntToStr (Request.ContentLength) + '<br>'#13 +
    'ContentVersion: ' + Request.ContentVersion + '<br>'#13 +
    'Content: ' + Request.Content + '<br>'#13 +
    'Connection: ' + Request.Connection + '<br>'#13 +
    'DerivedFrom: ' + Request.DerivedFrom + '<br>'#13 +
    'Expires: ' + DateTimeToStr (Request.Expires) + '<br>'#13 +
    'Title: ' + Request.Title + '<br>'#13 +
    'RemoteAddr: ' + Request.RemoteAddr + '<br>'#13 +
    'RemoteHost: ' + Request.RemoteHost + '<br>'#13 +
    'ServerPort: ' + IntToStr (Request.ServerPort) + '<br>'#13;
  // list of strings
  Response.Content := Response.Content +
    'ContentFields:<ul>'#13;
  for I := 0 to Request.ContentFields.Count - 1 do
    Response.Content := Response.Content +
      '<li>' + Request.ContentFields [I]+ #13;
  Response.Content := Response.Content +
    '</ul>CookieFields:<ul>'#13;
  for I := 0 to Request.CookieFields.Count - 1 do
    Response.Content := Response.Content +
      '<li>' + Request.CookieFields [I] + #13;
  Response.Content := Response.Content +
    '</ul>QueryFields:<ul>'#13;
  for I := 0 to Request.QueryFields.Count - 1 do
    Response.Content := Response.Content +
      '<li>' + Request.QueryFields [I] + #13;
end;

procedure TWebModule1.dsTableProcedureFormatCell(Sender: TObject; CellRow,
  CellColumn: Integer; var BgColor: THTMLBgColor; var Align: THTMLAlign;
  var VAlign: THTMLVAlign; var CustomAttrs, CellData: string);
begin
  if (CellColumn = 0) and (CellRow <> 0) then
    CellData := '<a href="' + '/record?ID=' +
      EmployeeTable.FieldByName('EMP_NO').AsString + '"> ' + CellData + ' </a>';
end;

procedure TWebModule1.MenuAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  i: Integer;
begin
  Response.Content := '<H3>Menu</H3><ul>'#13;
  for I := 0 to Actions.Count - 1 do
    Response.Content := Response.Content + '<li> <a href="' + Action[I].PathInfo
      + '"> ' + Copy(Action[I].Name, 3, 1000) + '</a>'#13;
  Response.Content := Response.Content + '</ul>';
end;

procedure TWebModule1.RecordAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  EmployeeTable.Open;
  EmployeeTable.FindKey([Request.QueryFields.Values['ID']]);
  Response.Content := dsPageProducer.Content;
end;

procedure TWebModule1.TableAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  EmployeeTable.Open;
  EmployeeTable.First;
end;

end.
