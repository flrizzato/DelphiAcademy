unit Files_DataModule;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, EMS.FileResource;

type
  [ResourceName('files')]
  TFilesResource1 = class(TDataModule)
    [ResourceSuffix('list', '/')]
    [ResourceSuffix('get', '/{id}')]
    EMSFileResource1: TEMSFileResource;
  published
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure Register;
begin
  RegisterResource(TypeInfo(TFilesResource1));
end;

initialization
  Register;
end.


