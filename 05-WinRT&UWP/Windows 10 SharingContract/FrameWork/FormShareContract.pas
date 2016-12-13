//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FormShareContract;

interface

uses
  System.Win.ShareContract, System.Win.WinRT, System.Classes, WinAPI.ApplicationModel.DataTransfer,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ShareContract, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
  TFormSharingContract = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    ButtonShare: TButton;
    EditShareText: TEdit;
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
    procedure ShareContractAppChosen(const Sender: TObject; const AManager: IDataTransferManager;
      const Args: ITargetApplicationChosenEventArgs);
    procedure ShareContractTranferImage(const Sender: TObject; const ARequest: IDataProviderRequest);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FShareWrapper: TShareContract;
  public
    { Public declarations }
  end;

var
  FormSharingContract: TFormSharingContract;

implementation

uses
  System.SysUtils,
  System.IOUtils;

{$R *.dfm}


procedure TFormSharingContract.ButtonShareClick(Sender: TObject);
begin
  // Set the shared properties
  FShareWrapper.ApplicationName := EditApplicationName.Text;
  FShareWrapper.Description := EditDescription.Text;
  FShareWrapper.DataTitle := EditDataTitle.Text;
  FShareWrapper.DataText := EditShareText.Text;
  FShareWrapper.PackageName := EditPackageName.Text;
  FShareWrapper.ContentSourceApplicationLink := EditContentSourceApplicationLink.Text;
  FShareWrapper.ContentSourceWebLink := EditContentSourceWebLink.Text;

  FShareWrapper.IconFile := 'Penguins.bmp';
  FShareWrapper.ImageFile := 'Penguins.jpg';
  FShareWrapper.LogoFile := 'Penguins.bmp';


  FShareWrapper.RtfText := 'This is the RTF Text. Should be a large RTF text that is shared...';

  FShareWrapper.HTML := '<p>Here is our store logo: <img src=''Penguins.bmp''>.</p>';

  FShareWrapper.WebAddress := EditWebAddress.Text;

  // Launch Sharing process. Shows applications that can receive our shared information.
  FShareWrapper.InitSharing;
end;

procedure TFormSharingContract.FormCreate(Sender: TObject);
begin
  TShareContract.OnProcessMessages := Application.ProcessMessages;
  TShareContract.BasePath := ExtractFilePath(Application.ExeName);
  FShareWrapper := TShareContract.Create(Self.Handle);
  FShareWrapper.OnAppChosen := ShareContractAppChosen;
  FShareWrapper.OnTransferImage := ShareContractTranferImage;
end;

procedure TFormSharingContract.FormDestroy(Sender: TObject);
begin
  FShareWrapper.Free;
end;

procedure TFormSharingContract.ShareContractAppChosen(const Sender: TObject; const AManager: IDataTransferManager;
  const Args: ITargetApplicationChosenEventArgs);
begin
  // With this event we can know which application is going to receive our data.
  Memo1.Lines.Add('Application Chosen: ' + args.ApplicationName.ToString);
end;

procedure TFormSharingContract.ShareContractTranferImage(const Sender: TObject; const ARequest: IDataProviderRequest);
begin
  Memo1.Lines.Add('Transfering Image');
  // We must provide the stream with the data, the source of the stream can be any we can imagine. Hence the event
  // to retrieve it.
  // For testing purposes we do the same that we do in the TShareContract class.
  ARequest.SetData(TShareContract.FileNameToStream(TShareContract(Sender).ImageFile));
end;

end.
