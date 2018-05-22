{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.DlgPushDataU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, IPPeerClient, REST.Backend.PushTypes,
  REST.Backend.MetaTypes, System.JSON, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Edit, FMX.Layouts, FMX.Memo, FMX.ScrollBox;

type
  TDlgPushData = class(TForm)
    EditMessage: TEdit;
    LabelMessage: TLabel;
    LinkControlToFieldMessage: TLinkControlToField;
    BindingsList1: TBindingsList;
    EditAPSBadge: TEdit;
    LabelAPSBadge: TLabel;
    LinkControlToFieldAPSBadge: TLinkControlToField;
    EditAPSSound: TEdit;
    LabelAPSSound: TLabel;
    LinkControlToFieldAPSSound: TLinkControlToField;
    EditGCMTitle: TEdit;
    LabelGCMTitle: TLabel;
    LinkControlToFieldGCMTitle: TLinkControlToField;
    GroupBoxAPNS: TGroupBox;
    GroupBoxGCM: TGroupBox;
    BackendPush1: TBackendPush;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    EditGCMMessage: TEdit;
    Label1: TLabel;
    LinkControlToField2: TLinkControlToField;
    Edit1: TEdit;
    LinkControlToField3: TLinkControlToField;
    Label2: TLabel;
  private
    FJSONData: TJSONObject;
    function GetJSONData: TJSONObject;
    procedure SetJSONData(const Value: TJSONObject);
    { Private declarations }
  public
    destructor Destroy; override;
    { Public declarations }
    property JSONData: TJSONObject read GetJSONData write SetJSONData;
  end;

var
  DlgPushData: TDlgPushData;

implementation

{$R *.fmx}

{ TDlgPushData }

destructor TDlgPushData.Destroy;
begin
  FJSONData.Free;
  inherited;
end;

function TDlgPushData.GetJSONData: TJSONObject;
begin
  if FJSONData = nil then
    FJSONData := TJSONObject.Create;
  if BackendPush1.Message <> '' then
    FJSONData.AddPair('message', BackendPush1.Message);
  BackendPush1.APS.Save(FJSONData, 'aps');
  BackendPush1.GCM.Save(FJSONData, 'gcm');
  Result := FJSONData;
end;

procedure TDlgPushData.SetJSONData(const Value: TJSONObject);
var
  LMessage: string;
begin
  BackendPush1.APS.Load(Value, 'aps');
  BackendPush1.GCM.Load(Value, 'gcm');
  if Value.TryGetValue<string>('message', LMessage) then
    BackendPush1.Message := LMessage;
//    LPushData.Extras.Load(Value, 'extras');
//    BackendPush
end;

end.
