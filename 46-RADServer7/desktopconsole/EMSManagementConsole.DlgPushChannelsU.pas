{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.DlgPushChannelsU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Generics.Collections, FMX.ListView.Types, FMX.StdCtrls, FMX.ListView, 
  FMX.ListView.Appearances, System.JSON, FMX.Controls.Presentation;

type
  TDlgPushChannels = class(TForm)
    GroupBox2: TGroupBox;
    CheckBoxAllChannels: TCheckBox;
    ListView1: TListView;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure CheckBoxAllChannelsChange(Sender: TObject);
  private
    FChannels: TArray<string>;
    FChecking: Boolean;
    procedure SetChannels(const Value: TArray<string>);
    procedure UpdateCheckAll;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Channels: TArray<string> write SetChannels;
    procedure Load(const AJSON: TJSONObject);
    procedure Save(const AJSON: TJSONObject);
  end;

var
  DlgPushChannels: TDlgPushChannels;

implementation

{$R *.fmx}

{ TDlgPushChannels }

procedure TDlgPushChannels.CheckBoxAllChannelsChange(Sender: TObject);
begin
  if not FChecking then
    ListView1.Items.CheckAll(CheckBoxAllChannels.IsChecked);
end;

constructor TDlgPushChannels.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TDlgPushChannels.Destroy;
begin
  inherited;
end;

procedure TDlgPushChannels.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  UpdateCheckAll;
end;

procedure TDlgPushChannels.Load(const AJSON: TJSONObject);
var
  LChannels: TJSONArray;
  LValue: TJSONValue;
  LDictionary: TDictionary<string, Integer>;
  S: string;
  I: Integer;
begin
  LDictionary := TDictionary<string, Integer>.Create;
  try
    for S in FChannels do
      LDictionary.Add(S, LDictionary.Count);
    if AJSON.TryGetValue<TJSONArray>('channels', LChannels) then
      for LValue in LChannels do
        if LDictionary.TryGetValue(LValue.Value, I) then
          ListView1.Items[I].Checked := True;
    UpdateCheckAll;
  finally
    LDictionary.Free;
  end;
end;

procedure TDlgPushChannels.UpdateCheckAll;
begin
  FChecking := True;
  try
    CheckBoxAllChannels.IsChecked := ListView1.Items.CheckedCount = ListView1.Items.Count;
  finally
    FChecking := False;
  end;
end;

procedure TDlgPushChannels.Save(const AJSON: TJSONObject);
var
  LJSONChannels: TJSONArray;
  LItem: TListViewItem;
begin
  AJSON.RemovePair('channels');
  LJSONChannels := TJSONArray.Create;
  for LItem in ListView1.Items do
    if LItem.Checked then
      LJSONChannels.Add(FChannels[LItem.Index]);
  AJSON.AddPair('channels', LJSONChannels);
end;

procedure TDlgPushChannels.SetChannels(const Value: TArray<string>);
var
  S: string;
begin
  FChannels := Value;
  for S in FChannels do
    ListView1.Items.Add.Text := S;

end;

end.
