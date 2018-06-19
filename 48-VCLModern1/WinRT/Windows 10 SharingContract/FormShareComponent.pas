//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FormShareComponent;

interface

uses
  System.Classes, System.ImageList, System.Win.ShareContract, System.Win.WinRT, WinAPI.ApplicationModel.DataTransfer,
  Vcl.Forms, Vcl.ShareContract, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFShareComponent = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    ButtonShare: TButton;
    EditShareText: TEdit;
    ShareContractComponent: TSharingContract;
    Label2: TLabel;
    EditApplicationName: TEdit;
    Label3: TLabel;
    EditDescription: TEdit;
    Label4: TLabel;
    EditPackageName: TEdit;
    Label5: TLabel;
    EditWebAddress: TEdit;
    Label6: TLabel;
    EditContentSourceWebLink: TEdit;
    Label7: TLabel;
    EditContentSourceApplicationLink: TEdit;
    Label8: TLabel;
    EditDataTitle: TEdit;
    ImageList1: TImageList;
    procedure ButtonShareClick(Sender: TObject);
    procedure ShareContractComponentAppChosen(const Sender: TObject; const AManager: IDataTransferManager;
      const Args: ITargetApplicationChosenEventArgs);
    procedure ShareContractComponentTransferImage(const Sender: TObject;
      const ARequest: IDataProviderRequest);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FShareComponent: TFShareComponent;

implementation

uses
  System.SysUtils, System.IOUtils;

{$R *.dfm}


procedure TFShareComponent.ButtonShareClick(Sender: TObject);
begin
  // Set the shared properties
  ShareContractComponent.ApplicationName := EditApplicationName.Text;
  ShareContractComponent.Description := EditDescription.Text;
  ShareContractComponent.DataTitle := EditDataTitle.Text;
  ShareContractComponent.DataText := EditShareText.Text;
  ShareContractComponent.PackageName := EditPackageName.Text;
  ShareContractComponent.ContentSourceApplicationLink := EditContentSourceApplicationLink.Text;
  ShareContractComponent.ContentSourceWebLink := EditContentSourceWebLink.Text;

  ShareContractComponent.IconFile := 'Penguins.bmp';
  ShareContractComponent.ImageFile := 'Penguins.jpg';
  ShareContractComponent.LogoFile := 'Penguins.bmp';


  ShareContractComponent.RtfText := 'This is the RTF Text. Should be a large RTF text that is shared...';

  ShareContractComponent.HTML := '<p>Here is our store logo: <img src=''Penguins.bmp''>.</p>';

  ShareContractComponent.WebAddress := EditWebAddress.Text;

  // Launch Sharing process. Shows applications that can receive our shared information.
  ShareContractComponent.InitSharing;
end;


procedure TFShareComponent.ShareContractComponentAppChosen(const Sender: TObject; const AManager: IDataTransferManager;
  const Args: ITargetApplicationChosenEventArgs);
begin
  // With this event we can know which application is going to receive our data.
  Memo1.Lines.Add('Application Chosen: ' + args.ApplicationName.ToString);
end;

procedure TFShareComponent.ShareContractComponentTransferImage(
  const Sender: TObject; const ARequest: IDataProviderRequest);
begin
  // We must provide the stream with the data, the source of the stream can be any we can imagine. Hence the event
  // to retrieve it.
  // For testing purposes we do the same that we do in the TShareContract class.
  ARequest.SetData(TShareContract.FileNameToStream(TShareContract(Sender).ImageFile));
end;

end.
