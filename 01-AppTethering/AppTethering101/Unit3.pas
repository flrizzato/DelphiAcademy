unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Controls.Presentation, FMX.Edit, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile;

type
  TForm3 = class(TForm)
    Edit1: TEdit;
    ListBox1: TListBox;
    Button1: TButton;
    ActionList1: TActionList;
    Action1: TAction;
    TetheringAppProfile1: TTetheringAppProfile;
    TetheringManager1: TTetheringManager;
    procedure Action1Execute(Sender: TObject);
    procedure TetheringAppProfile1ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Action1Execute(Sender: TObject);
begin
  ListBox1.Items.Add(Edit1.Text);
end;

procedure TForm3.TetheringAppProfile1ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
begin
  if AResource.Hint = 'MyText' then
    Edit1.Text := AResource.Value.AsString;
end;

end.
