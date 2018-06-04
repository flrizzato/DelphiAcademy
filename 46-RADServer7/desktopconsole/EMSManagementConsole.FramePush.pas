{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.FramePush;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, System.JSON, System.Generics.Collections, FMX.ListView.Types,
  FMX.ListView, FMX.Memo, EMSManagementConsole.Types, EMSManagementConsole.DlgPushWhereU,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TPushFrame = class(TFrame)
    Layout1: TLayout;
    ButtonPushData: TButton;
    ButtonTargetChannels: TButton;
    ButtonTargetWhere: TButton;
    SendButton: TButton;
    Layout2: TLayout;
    LayoutPushData: TLayout;
    DataMemo: TMemo;
    TitleLabel: TLabel;
    LayoutPushTarget: TLayout;
    TargetMemo: TMemo;
    Label4: TLabel;
    Splitter1: TSplitter;
    Layout3: TLayout;
    ListView1: TListView;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ClearButton: TButton;
    procedure ClearButtonClick(Sender: TObject);
    procedure ButtonPushDataClick(Sender: TObject);
    procedure ButtonTargetChannelsClick(Sender: TObject);
    procedure ButtonTargetWhereClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  public type
    TSendStatus = record
    private
      FIOSCount: Integer;
      FAndroidCount: Integer;
    public
      constructor Create(AIOSCount, AAndroidCount: Integer);
    end;
    THistoryItem = class
    private
      FSendTime: TDateTime;
      FPayload: TJSONObject;
      FTarget: TJSONObject;
      FSendStatus: TSendStatus;
      FMessage: string;
    public
      constructor Create(ASendTime: TDateTime; const AMessage: string; const APayload, ATarget: TJSONObject;
        const ASendStatus: TSendStatus);
      function ToString: string; override;
      property Payload: TJSONObject read FPayload;
      property Target: TJSONObject read FTarget;
    end;
  private
    { Private declarations }
    FItems: TList<THistoryItem>;
    FOnSelect: TNotifyEvent;
    FAdding: Boolean;
    function GetSelected: THistoryItem;
    procedure UpdateList;
    procedure AddListItem(const AItem: THistoryItem);
    procedure GetDevices(out ADevices: TDlgPushWhere.TDevices);
    procedure GetChannels(out AChannels: TArray<string>);
    procedure OnHistorySelect(ASender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const AItem: THistoryItem);
    procedure Clear;
    property Selected: THistoryItem read GetSelected;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation

uses
  REST.Json, EMSManagementConsole.ModuleBackend, REST.Backend.EMSApi,
  Rest.Backend.PushTypes, EMSManagementConsole.Consts,
  REST.Backend.EMSProvider, EMSManagementConsole.DlgPushDataU,
  EMSManagementConsole.DlgPushChannelsU, EMSManagementConsole.Form;

{$R *.fmx}

constructor TPushFrame.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList<THistoryItem>.Create;
  OnSelect := OnHistorySelect;
end;

destructor TPushFrame.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TPushFrame.GetChannels(out AChannels: TArray<string>);
var
  LEMSClientAPI: TEMSClientAPI;
begin
  LEMSClientApi := (DataModule1.BackendPush1.ProviderService as IGetEMSApi).EMSAPI;
  AChannels := LEMSClientApi.RetrieveInstallationsChannelNames;
end;

procedure TPushFrame.GetDevices(out ADevices: TDlgPushWhere.TDevices);
var
  LJSONArray: TJSONArray;
  LJSONValue: TJSONValue;
  I: Integer;
  LDevice: TDlgPushWhere.TDevice;
  LEMSClientAPI: TEMSClientAPI;
begin
  LJSONArray := TJSONArray.Create;
  try
    LEMSClientApi := (DataModule1.BackendPush1.ProviderService as IGetEMSApi).EMSAPI;
    LEMSClientApi.QueryInstallations([], LJSONArray);
    SetLength(ADevices, LJSONArray.Count);
    for I := 0 to LJSONArray.Count - 1 do
    begin
      LJSONValue := LJSONArray.Items[I];
      LDevice := TDlgPushWhere.TDevice.Create(
        LJSONValue.GetValue<string>('deviceToken', ''),
        LJSONValue.GetValue<string>('deviceType', ''),
        []);
      ADevices[I] := LDevice;
    end;
  finally
    LJSONArray.Free;
  end;
end;

function TPushFrame.GetSelected: THistoryItem;
begin
  if ListView1.ItemIndex >= 0 then
    Result := FItems[ListView1.ItemIndex]
  else
    Result := nil;
end;

procedure TPushFrame.ListView1Change(Sender: TObject);
begin
  if not FAdding then
    if Assigned(FOnSelect) then
      FOnSelect(Self);
end;

procedure TPushFrame.OnHistorySelect(ASender: TObject);
var
  LItem: TPushFrame.THistoryItem;
begin
  LItem := Selected;
  if LItem <> nil then
  begin
    if LItem.PayLoad <> nil then
      DataMemo.Text := TJson.Format(LItem.Payload)
    else
      DataMemo.Text := '';

    if LItem.Target <> nil then
      TargetMemo.Text := TJson.Format(LItem.Target)
    else
      TargetMemo.Text := '';
  end;
end;

procedure TPushFrame.SendButtonClick(Sender: TObject);
var
  LPushData: TPushData;
  LTarget: TJSONValue;
  LData: TJSONValue;
  LTargetText: string;
  LDataText: string;
  LErrorMessage: string;
  LPushStatus: TEMSClientApi.TPushStatus;
  LJSONPushData: TJSONObject;
  LEMSClientAPI: TEMSClientAPI;
  LMessages: string;
begin
  LTarget := nil;
  LData := nil;
  LPushData := nil;
  try
    LTargetText := Trim(TargetMemo.Text);
    if LTargetText <> '' then
    begin
      LTarget := TJSONObject.ParseJSONValue(LTargetText);
      if not (LTarget is TJSONObject) then
        raise Exception.Create(strTargetNotJSONObject);
    end;
    LDataText := Trim(DataMemo.Text);
    LData := TJSONObject.ParseJSONValue(LDataText);
    if not (LData is TJSONObject) then
      raise Exception.Create(strTargetNotJSONObject);
    LPushData := TPushData.Create;
    try
      LPushData.Load(TJSONObject(LData));
      try
        LJSONPushData := DataModule1.BackendPush1.PushAPI.PushDataAsJSON(LPushData);
        try
          LEMSClientApi := (DataModule1.BackendPush1.ProviderService as IGetEMSApi).EMSAPI;
          LEMSClientApi.PushToTarget(LJSONPushData, TJSONObject(LTarget), LPushStatus);
        finally
          LJSONPushData.Free;
        end;
      except
        On E: Exception do
        begin
          LErrorMessage := E.Message;
          raise;
        end;
      end;
    finally
      LMessages := ' - Message: ''' + LPushData.Message + ''' - APS.Alert: ''' + LPushData.APS.Alert + ''' - GCM.Message: ''' + LPushData.GCM.Message + '''';
      Add(
        THistoryItem.Create(Now, LMessages, TJSONObject(LData), TJSONObject(LTarget),
          TSendStatus.Create(LPushStatus.QueuedIOS, LPushStatus.QueuedAndroid)));
    end;
  finally
    LPushData.Free;
    LTarget.Free;
    LData.Free;
  end;
end;
procedure TPushFrame.UpdateList;
var
  LItem: THistoryItem;
begin
  ListView1.BeginUpdate;
  try
    for LItem in FItems do
      AddListItem(LItem);
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TPushFrame.Add(const AItem: THistoryItem);
begin
  FItems.Add(AItem);
  AddListItem(AItem);
end;

procedure TPushFrame.AddListItem(const AItem: THistoryItem);
begin
  FAdding := True;
  try
    ListView1.Items.Add.Text := AItem.ToString;
    ListView1.ItemIndex := ListView1.Items.Count - 1;
  finally
    FAdding := False;
  end;
end;

procedure TPushFrame.ButtonPushDataClick(Sender: TObject);
var
  LJSONData: TJSONValue;
  LDlg: TDlgPushData;
  LData: string;
begin
  LJSONData := nil;
  try
    LData := Trim(DataMemo.Text);
    if LData <> '' then
    begin
      LJSONData := TJSONObject.ParseJSONValue(LData);
      if LJSONData = nil then
        raise Exception.Create(strDataNotJSON);
      if not (LJSONData is TJSONObject) then
        raise Exception.Create(strDataNotJSONObject);
    end;

    LDlg := TDlgPushData.Create(Self);
    try
      if LJSONData <> nil then
        LDlg.JSONData := TJSONObject(LJSONData);
      if LDlg.ShowModal = mrOk then
      begin
        FreeAndNil(LJSONData);
        LJSONData := LDlg.JSONData.Clone as TJSONValue;
        DataMemo.Text := Rest.Json.TJson.Format(LJSONData);
      end;
    finally
      LDlg.Free;
    end;
  finally
    LJSONData.Free;
  end;
end;

procedure TPushFrame.ButtonTargetChannelsClick(Sender: TObject);
var
  LJSONData: TJSONValue;
  LDlg: TDlgPushChannels;
  LTarget: string;
  LChannels: TArray<string>;
begin
  if DataModule1.EMSProvider.BaseURL <> '' then
  begin
    LJSONData := nil;
    try
      LTarget := Trim(TargetMemo.Text);
      if LTarget <> '' then
      begin
        LJSONData := TJSONObject.ParseJSONValue(LTarget);
        if LJSONData = nil then
          raise Exception.Create(strTargetNotJSON);
        if not (LJSONData is TJSONObject) then
          raise Exception.Create(strTargetNotJSONObject);
      end;
      if LJSONData = nil then
        LJSONData := TJSONObject.Create;
      LDlg := TDlgPushChannels.Create(Self);
      try
        GetChannels(LChannels);
        LDlg.Channels := LChannels;
        LDlg.Load(TJSONObject(LJSONData));
        if LDlg.ShowModal = mrOk then
        begin
          LDlg.Save(TJSONObject(LJSONData));
          TargetMemo.Text := Rest.Json.TJson.Format(LJSONData);
        end;
      finally
        LDlg.Free;
      end;
    finally
      LJSONData.Free;
    end;
  end
  else
    raise Exception.Create(strURLBlank);
end;

procedure TPushFrame.ButtonTargetWhereClick(Sender: TObject);
var
  LJSONData: TJSONValue;
  LDlg: TDlgPushWhere;
  LTarget: string;
  LDevices: TDlgPushWhere.TDevices;
begin
  if DataModule1.EMSProvider.BaseURL <> '' then
  begin
    LJSONData := nil;
    try
      LTarget := Trim(TargetMemo.Text);
      if LTarget <> '' then
      begin
        LJSONData := TJSONObject.ParseJSONValue(LTarget);
        if LJSONData = nil then
          raise Exception.Create(strTargetNotJSON);
        if not (LJSONData is TJSONObject) then
          raise Exception.Create(strTargetNotJSONObject);
      end;
      if LJSONData = nil then
        LJSONData := TJSONObject.Create;
      LDlg := TDlgPushWhere.Create(Self);
      try
        GetDevices(LDevices);
        LDlg.Devices := LDevices;
        LDlg.Load(TJSONObject(LJSONData));
        if LDlg.ShowModal = mrOk then
        begin
          LDlg.Save(TJSONObject(LJSONData));
          TargetMemo.Text := Rest.Json.TJson.Format(LJSONData);
        end;
      finally
        LDlg.Free;
      end;
    finally
      LJSONData.Free;
    end;
  end
  else
    raise Exception.Create(strURLBlank);
end;

procedure TPushFrame.Clear;
begin
  ListView1.Items.Clear;
  FItems.Clear;
  UpdateList;
end;

procedure TPushFrame.ClearButtonClick(Sender: TObject);
begin
  Clear;
end;

procedure TPushFrame.CloseButtonClick(Sender: TObject);
begin
  Clear;
  Visible := False;
  TForm2(Root).ViewsFrame.Visible := True;
  DataMemo.Text := '';
  TargetMemo.Text := '';
end;

{ TFrame1.TSendStatus }

constructor TPushFrame.TSendStatus.Create(AIOSCount, AAndroidCount: Integer);
begin
  FIOSCount := AIOSCount;
  FAndroidCount := AAndroidCount;
end;

{ TFrame1.THistoryItem }

constructor TPushFrame.THistoryItem.Create(ASendTime: TDateTime;
  const AMessage: string; const APayload, ATarget: TJSONObject;
  const ASendStatus: TSendStatus);
begin
  FMessage := AMessage;
  FSendTime := ASendTime;
  if APayload <> nil then
    FPayload := APayload.Clone as TJSONObject;
  if ATarget <> nil then
    FTarget := ATarget.Clone as TJSONObject;
  FSendStatus := ASendStatus;
end;

function TPushFrame.THistoryItem.ToString: string;
begin
  Result := TimeToStr(FSendTime) + Format(strMessageQueued, [FMessage,
    FSendStatus.FAndroidCount,
    FSendStatus.FIOSCount]);
end;

end.

