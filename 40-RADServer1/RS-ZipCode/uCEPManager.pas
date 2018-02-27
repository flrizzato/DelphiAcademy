unit uCEPManager;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.JSON.Types,
  System.JSON.Writers,
  System.JSON.Readers,
  AtendeCliente1;

type
  TMyCEP = class(TObject)
    fCEP: string;
    fLogradouro: string;
    fBairro: string;
    fCidade: string;
    fUF: string;
  end;

type
  TCEPManager = class(TObject)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    class function CEP2JSON(eERP: enderecoERP): TJSONObject;
    class function JSON2CEP(jsonCEP: string): TMyCEP;
  end;

implementation

{ TCEPManager }

class function TCEPManager.CEP2JSON(eERP: enderecoERP): TJSONObject;
var
  StringWriter: TStringWriter;
  Writer: TJsonTextWriter;
begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter);
  try
    Writer.Formatting := TJsonFormatting.Indented;

    Writer.WriteStartObject;
    Writer.WritePropertyName('CEP');
    Writer.WriteValue(eERP.cep);
    Writer.WritePropertyName('Logradouro');
    Writer.WriteValue(eERP.end_);
    Writer.WritePropertyName('Bairro');
    Writer.WriteValue(eERP.bairro);
    Writer.WritePropertyName('Cidade');
    Writer.WriteValue(eERP.cidade);
    Writer.WritePropertyName('UF');
    Writer.WriteValue(eERP.uf);
    Writer.WriteEndObject;

    Result := TJSONObject.ParseJSONValue
      (TEncoding.ASCII.GetBytes(StringWriter.ToString), 0) as TJSONObject;
  finally
    Writer.Free;
    StringWriter.Free;
  end;
end;

class function TCEPManager.JSON2CEP(jsonCEP: string): TMyCEP;
var
  StringReader: TStringReader;
  Reader: TJsonTextReader;
begin
  Result := TMyCEP.Create;
  StringReader := TStringReader.Create(jsonCEP);
  Reader := TJsonTextReader.Create(StringReader);
  try
    while Reader.Read do
    begin
      if Reader.TokenType = TJsonToken.PropertyName then
      begin
        if Reader.Value.AsString = 'CEP' then
          Result.fCEP := Reader.ReadAsString;
        if Reader.Value.AsString = 'Logradouro' then
          Result.fLogradouro := Reader.ReadAsString;
        if Reader.Value.AsString = 'Bairro' then
          Result.fBairro := Reader.ReadAsString;
        if Reader.Value.AsString = 'Cidade' then
          Result.fCidade := Reader.ReadAsString;
        if Reader.Value.AsString = 'UF' then
          Result.fUF := Reader.ReadAsString;
      end;
    end;
  finally
    StringReader.Free;
    Reader.Free;
  end;
end;

end.
