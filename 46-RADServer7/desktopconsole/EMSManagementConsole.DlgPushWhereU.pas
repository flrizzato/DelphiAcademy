{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.DlgPushWhereU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.JSON, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  System.Generics.Collections, FMX.ListView.Types, FMX.ListView, FMX.ListView.Appearances,
  EMSManagementSetUp.QueryU, FMX.Controls.Presentation, FMX.ListView.Adapters.Base;

type
  TDlgPushWhere = class(TForm)
    GroupBox1: TGroupBox;
    RadioButtonAll: TRadioButton;
    RadioButtonIOS: TRadioButton;
    RadioButtonAndroid: TRadioButton;
    GroupBox2: TGroupBox;
    CheckBoxAllDevices: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ListView1: TListView;
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure RadioButtonAllChange(Sender: TObject);
    procedure CheckBoxAllDevicesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    type
      TDevice = record
        DeviceToken: string;
        DeviceType: string;
        Channels: TArray<string>;
      public
        constructor Create(const ADeviceToken, ADeviceType: string; const AChannels: TArray<string>);
      end;
      TDevices = TArray<TDevice>;
  private
    FChecking: Boolean;
    FDevices: TDevices;
    FDeviceTokens: TDictionary<string, Boolean>;
    FFilters: TFilters;
    procedure ListCheckDeviceTokens;
    procedure CheckDeviceType(const ADeviceType: string);
    procedure SetDevices(const Value: TDevices);
    procedure ListDevices;
//    procedure UpdateDeviceTokens(const AArray: TJSONArray); overload;
    procedure UpdateDeviceTokens(const AArray: TArray<string>); overload;
    procedure UpdateCheckBoxAllDevices;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AJSONQuery: TJSONObject);
    procedure Save(const AJSONQuery: TJSONObject);
    property Devices: TDevices write SetDevices;
  end;

var
  DlgPushWhere: TDlgPushWhere;

implementation

{$R *.fmx}

uses System.Rtti;

const
  sDeviceType = 'deviceType';
  sDeviceToken = 'deviceToken';
  sWhere = 'where';
  sIOS = 'ios';
  sAndroid = 'android';


procedure TDlgPushWhere.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  I: Integer;
begin
  I := AItem.Tag;
  FDeviceTokens[FDevices[I].DeviceToken] := AItem.Checked;
  if ListView1.Items.AnyChecked(True) then
    CheckBoxAllDevices.IsChecked := False
  else
    CheckBoxAllDevices.IsChecked := True;
end;

procedure TDlgPushWhere.Load(const AJSONQuery: TJSONObject);
var
  LWhere: TJSONObject;
  I: Integer;
  LStrings: TArray<string>;
  LValue: TValue;
  LSimpleFilter: TSimpleFilter;
  LInFilter: TInFilter;
begin
  FChecking := True;
  try
    FFilters := TFilters.Create;
    if AJSONQuery.TryGetValue<TJSONObject>('where', LWhere) then
    begin
      TJSONFilterParser.Parse(LWhere, FFilters, 0);
      for I := 0 to FFilters.Count - 1 do
      begin
        LSimpleFilter := nil;
        LInFilter := nil;
        if FFilters.Items[I] is TSimpleFilter then
          LSimpleFilter := TSimpleFilter(FFilters.Items[I])
        else if FFilters.Items[I] is TInFilter then
          LInFilter := TInFilter(FFilters.Items[I]);
        if (LSimpleFilter <> nil) and (LSimpleFilter.Op = TSimpleFilter.TOperator.eq) then
        begin
          if LSimpleFilter.Attribute = sDeviceType then
            CheckDeviceType(LSimpleFilter.Value.AsString);
          if LSimpleFilter.Attribute = sDeviceToken then
            UpdateDeviceTokens(TArray<string>.Create(LSimpleFilter.Value.AsString));
        end
        else if (LInFilter <> nil) then
        begin
          if LInFilter.Attribute = 'deviceToken' then
          begin
            LStrings := nil;
            for LValue in TInFilter(FFilters.Items[I]).Values do
              LStrings := LStrings + [LValue.AsString];
            UpdateDeviceTokens(LStrings);
          end;
        end;
      end;
    end
    else
      RadioButtonAll.IsChecked := True;
    UpdateCheckBoxAllDevices;
  finally
    FChecking := False;
  end;
end;

procedure TDlgPushWhere.UpdateCheckBoxAllDevices;
begin
  FChecking := True;
  try
    CheckBoxAllDevices.IsChecked := not ListView1.Items.AnyChecked;
  finally
    FChecking := False;
  end;
end;

procedure TDlgPushWhere.RadioButtonAllChange(Sender: TObject);
begin
  if not FChecking then
  begin
    ListDevices;
    ListCheckDeviceTokens;
    UpdateCheckBoxAllDevices;
  end;
end;

procedure TDlgPushWhere.Save(const AJSONQuery: TJSONObject);
var
  LJSONWhere: TJSONObject;
  LPair: TPair<string, Boolean>;
  I: Integer;
  LAttribute: string;
  LSimpleFilter: TSimpleFilter;
  LInFilter: TInFilter;
  LValues: TList<TValue>;
begin
  for I := FFilters.Count - 1 downto 0 do
  begin
    LSimpleFilter := nil;
    LInFilter := nil;
    if FFilters.Items[I] is TSimpleFilter then
      LSimpleFilter := TSimpleFilter(FFilters.Items[I])
    else if FFilters.Items[I] is TInFilter then
      LInFilter := TInFilter(FFilters.Items[I]);
    if LSimpleFilter <> nil then
      LAttribute :=  LSimpleFilter.Attribute
    else if LInFilter <> nil then
      LAttribute :=  LInFilter.Attribute
    else
      continue;
    if LAttribute = sDeviceType then
      FFilters.Delete(I)
    else if LAttribute = sDeviceToken then
      FFilters.Delete(I);
  end;
  if RadioButtonIOS.IsChecked then
  begin
    LSimpleFilter := TSimpleFilter.Create;
    FFilters.Add(LSimpleFilter);
    LSimpleFilter.Attribute := sDeviceType;
    LSimpleFilter.Op := TSimpleFilter.TOperator.eq;
    LSimpleFilter.Value := sIOS;
  end
  else if RadioButtonAndroid.IsChecked then
  begin
    LSimpleFilter := TSimpleFilter.Create;
    FFilters.Add(LSimpleFilter);
    LSimpleFilter.Attribute := sDeviceType;
    LSimpleFilter.Op := TSimpleFilter.TOperator.eq;
    LSimpleFilter.Value := sAndroid;
  end;
  if not CheckBoxAllDevices.IsChecked then
  begin
    LInFilter := TInFilter.Create;
    FFilters.Add(LInFilter);
    LInFilter.Attribute := sDeviceToken;
    LInFilter.Op := TInFilter.TOperator.inop;
    LValues := TList<TValue>.Create;
    try
      for LPair in FDeviceTokens do
        if LPair.Value then
          LValues.Add(LPair.Key);
      LInFilter.Values := LValues.ToArray;
    finally
      LValues.Free;
    end;
  end;
  AJSONQuery.RemovePair(sWhere);
  LJSONWhere := TJSONObject.Create;
  AJSONQuery.AddPair(sWhere, LJSONWhere);
  TQueryToJSON.WriteFilters(FFilters, LJSONWhere);
end;


procedure TDlgPushWhere.SetDevices(const Value: TDevices);
var
  LDevice: TDevice;
begin
  FDevices := Value;
  for LDevice in Value do
    FDeviceTokens.Add(LDevice.DeviceToken, False);
  ListDevices;
end;

procedure TDlgPushWhere.ListDevices;
var
  LDevice: TDevice;
  LAdd: Boolean;
  I: Integer;
begin
  ListView1.BeginUpdate;
  try
    ListView1.Items.Clear;
    I := 0;
    for LDevice in FDevices do
    begin
      if RadioButtonIOS.IsChecked then
        LAdd := LDevice.DeviceType = sIOS
      else if RadioButtonAndroid.IsChecked then
        LAdd := LDevice.DeviceType = sAndroid
      else
        LAdd := True;
      if LAdd then
      begin
        with ListView1.Items.Add do
        begin
          Text := LDevice.DeviceToken;
          Tag := I;
        end;
      end;
      Inc(I);
    end;
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TDlgPushWhere.CheckDeviceType(const ADeviceType: string);
begin
  RadioButtonIOS.IsChecked := ADeviceType = sIOS;
  RadioButtonAndroid.IsChecked := ADeviceType = sAndroid;
  RadioButtonAll.IsChecked := (not RadioButtonIOS.IsChecked) and
    (not RadioButtonAndroid.IsChecked);
end;

constructor TDlgPushWhere.Create(AOwner: TComponent);
begin
  inherited;
  FFilters := TFilters.Create;
  FDeviceTokens := TDictionary<string, Boolean>.Create;
end;

destructor TDlgPushWhere.Destroy;
begin
  FDeviceTokens.Free;
  FFilters.Free;
  inherited;
end;

procedure TDlgPushWhere.FormShow(Sender: TObject);
begin
  if not FChecking then
  begin
    ListDevices;
    ListCheckDeviceTokens;
    UpdateCheckBoxAllDevices;
  end;
end;

//procedure TDlgPushWhere.UpdateDeviceTokens(const AArray: TJSONArray);
//var
//  LValue: TJSONValue;
//  S: string;
//begin
//  for S in FDeviceTokens.Keys do
//    FDeviceTokens[S] := False;
//  for LValue in AArray do
//    FDeviceTokens[LValue.Value] := True;
//  ListCheckDeviceTokens;
//end;

procedure TDlgPushWhere.UpdateDeviceTokens(const AArray: TArray<string>);
var
  S: string;
begin
  for S in FDeviceTokens.Keys do
    FDeviceTokens[S] := False;
  for S in AArray do
    FDeviceTokens[S] := True;
  ListCheckDeviceTokens;
end;

procedure TDlgPushWhere.CheckBoxAllDevicesChange(Sender: TObject);
var
  S: string;
begin
  if not FChecking then
  begin
    if CheckBoxAllDevices.IsChecked then
    begin
      for S in FDeviceTokens.Keys do
        FDeviceTokens[S] := False;
      ListCheckDeviceTokens;
    end;
    CheckBoxAllDevices.IsChecked := not ListView1.Items.AnyChecked;
  end
end;

procedure TDlgPushWhere.ListCheckDeviceTokens;
var
  LItem: TListViewItem;
  I: Integer;
begin
  for LItem in  ListView1.Items do
  begin
    I := LItem.Tag;
    LItem.Checked := FDeviceTokens[FDevices[I].DeviceToken];
  end;
end;

{ TDlgPushWhere.TDevice }

constructor TDlgPushWhere.TDevice.Create(const ADeviceToken, ADeviceType: string;
  const AChannels: TArray<string>);
begin
  DeviceToken := ADeviceToken;
  DeviceType := ADeviceType;
  Channels := AChannels;
end;

end.
