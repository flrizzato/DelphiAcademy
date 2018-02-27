// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl
//  >Import : https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl>0
// Encoding : UTF-8
// Version  : 1.0
// (7/2/2016 2:37:04 PM - - $Rev: 86412 $)
// ************************************************************************ //

unit AtendeCliente1;

interface

uses Soap.InvokeRegistry, Soap.SOAPHTTPClient, System.Types, Soap.XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_NLBL = $0004;
  IS_UNQL = $0008;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:base64Binary    - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:long            - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:dateTime        - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:unsignedShort   - "http://www.w3.org/2001/XMLSchema"[Gbl]

  cliente              = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  conta                = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  enderecoERP          = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  contratoERPPK        = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  unidadePostagemERP   = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  servicoSigep         = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  SQLException         = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Flt][GblElm] }
  Exception            = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Flt][GblElm] }
  AutenticacaoException = class;                { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Flt][Alias] }
  SigepClienteException = class;                { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Flt][Alias] }
  pedidoInformacao     = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  pedidoInformacaoRegistro = class;             { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  retorno              = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  clienteERP           = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  chancelaMaster       = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  gerenteConta         = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  servicoAdicionalERP  = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  vigenciaERP          = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  servicoERP           = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  contratoERP          = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  postagem             = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  pessoa               = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  remetente            = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  destinatario         = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  parametroMaster      = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  embalagemLRSMaster   = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  objetoSimplificadoTO = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  retornoCancelamentoTO = class;                { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  produtoTO            = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  pessoaTO             = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  remetenteTO          = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  assuntoPIMaster      = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  SQLException2        = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  Exception2           = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  cartaoPostagemERP    = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  motivoPIMaster       = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  objetoPostal         = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  usuarioInstalacao    = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  preListaPostagem     = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  objetoPostalPK       = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  objetoTO             = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  coletaTO             = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  coletaReversaTO      = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  coletaSimultaneaTO   = class;                 { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  pedidoInformacaoConsulta = class;             { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }
  mensagemRetornoPIMaster = class;              { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblCplx] }

  {$SCOPEDENUMS ON}
  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  statusGerente = (Ativo, Inativo);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  tipoGerente = (GerenteConta, GerenteContaMaster);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  categoriaServico = (SEM_CATEGORIA, PAC, SEDEX, CARTA_REGISTRADA, SERVICO_COM_RESTRICAO, REVERSO);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  statusCartao = (Desconhecido, Normal, Suspenso, Cancelado, Irregular);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  statusObjetoPostal = (EmBranco, Postado, Cancelado, Estorno);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  tipoBloqueio = (FRAUDE_BLOQUEIO, EXTRAVIO_VAREJO_PRE_INDENIZADO, EXTRAVIO_VAREJO_POS_INDENIZADO, EXTRAVIO_CORPORATIVO, INTERNACIONAL_LDI);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  statusPlp = (Aberta, Fechada, Postada);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  statusUsuario = (Ativo, Inativo);

  { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblSmpl] }
  acao = (DEVOLVIDO_AO_REMETENTE, ENCAMINHADO_PARA_REFUGO, REINTEGRADO_E_DEVOLVIDO_AO_REMETENTE, DESBLOQUEADO);

  {$SCOPEDENUMS OFF}

  Array_Of_parametroMaster = array of parametroMaster;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_servicoSigep = array of servicoSigep;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_objetoPostal = array of objetoPostal;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_objetoTO = array of objetoTO;        { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_produtoTO = array of produtoTO;      { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_pedidoInformacaoConsulta = array of pedidoInformacaoConsulta;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_servicoAdicionalERP = array of servicoAdicionalERP;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_pedidoInformacaoRegistro = array of pedidoInformacaoRegistro;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_contratoERP = array of contratoERP;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_usuarioInstalacao = array of usuarioInstalacao;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_unidadePostagemERP = array of unidadePostagemERP;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_clienteERP = array of clienteERP;    { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_gerenteConta = array of gerenteConta;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }
  Array_Of_cartaoPostagemERP = array of cartaoPostagemERP;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[GblUbnd] }


  // ************************************************************************ //
  // XML       : cliente, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  cliente = class(TRemotable)
  private
    FnumeroContrato: string;
    FnumeroContrato_Specified: boolean;
    FpossuiContrato: string;
    FpossuiContrato_Specified: boolean;
    procedure SetnumeroContrato(Index: Integer; const Astring: string);
    function  numeroContrato_Specified(Index: Integer): boolean;
    procedure SetpossuiContrato(Index: Integer; const Astring: string);
    function  possuiContrato_Specified(Index: Integer): boolean;
  published
    property numeroContrato: string  Index (IS_OPTN or IS_UNQL) read FnumeroContrato write SetnumeroContrato stored numeroContrato_Specified;
    property possuiContrato: string  Index (IS_OPTN or IS_UNQL) read FpossuiContrato write SetpossuiContrato stored possuiContrato_Specified;
  end;



  // ************************************************************************ //
  // XML       : conta, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  conta = class(TRemotable)
  private
    FcodigoBanco: string;
    FcodigoBanco_Specified: boolean;
    FnomeBanco: string;
    FnomeBanco_Specified: boolean;
    FnumeroAgencia: string;
    FnumeroAgencia_Specified: boolean;
    FnumeroConta: string;
    FnumeroConta_Specified: boolean;
    procedure SetcodigoBanco(Index: Integer; const Astring: string);
    function  codigoBanco_Specified(Index: Integer): boolean;
    procedure SetnomeBanco(Index: Integer; const Astring: string);
    function  nomeBanco_Specified(Index: Integer): boolean;
    procedure SetnumeroAgencia(Index: Integer; const Astring: string);
    function  numeroAgencia_Specified(Index: Integer): boolean;
    procedure SetnumeroConta(Index: Integer; const Astring: string);
    function  numeroConta_Specified(Index: Integer): boolean;
  published
    property codigoBanco:   string  Index (IS_OPTN or IS_UNQL) read FcodigoBanco write SetcodigoBanco stored codigoBanco_Specified;
    property nomeBanco:     string  Index (IS_OPTN or IS_UNQL) read FnomeBanco write SetnomeBanco stored nomeBanco_Specified;
    property numeroAgencia: string  Index (IS_OPTN or IS_UNQL) read FnumeroAgencia write SetnumeroAgencia stored numeroAgencia_Specified;
    property numeroConta:   string  Index (IS_OPTN or IS_UNQL) read FnumeroConta write SetnumeroConta stored numeroConta_Specified;
  end;



  // ************************************************************************ //
  // XML       : enderecoERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  enderecoERP = class(TRemotable)
  private
    Fbairro: string;
    Fbairro_Specified: boolean;
    Fcep: string;
    Fcep_Specified: boolean;
    Fcidade: string;
    Fcidade_Specified: boolean;
    Fcomplemento: string;
    Fcomplemento_Specified: boolean;
    Fcomplemento2: string;
    Fcomplemento2_Specified: boolean;
    Fend_: string;
    Fend__Specified: boolean;
    Fid: Int64;
    Fuf: string;
    Fuf_Specified: boolean;
    FunidadesPostagem: Array_Of_unidadePostagemERP;
    FunidadesPostagem_Specified: boolean;
    procedure Setbairro(Index: Integer; const Astring: string);
    function  bairro_Specified(Index: Integer): boolean;
    procedure Setcep(Index: Integer; const Astring: string);
    function  cep_Specified(Index: Integer): boolean;
    procedure Setcidade(Index: Integer; const Astring: string);
    function  cidade_Specified(Index: Integer): boolean;
    procedure Setcomplemento(Index: Integer; const Astring: string);
    function  complemento_Specified(Index: Integer): boolean;
    procedure Setcomplemento2(Index: Integer; const Astring: string);
    function  complemento2_Specified(Index: Integer): boolean;
    procedure Setend_(Index: Integer; const Astring: string);
    function  end__Specified(Index: Integer): boolean;
    procedure Setuf(Index: Integer; const Astring: string);
    function  uf_Specified(Index: Integer): boolean;
    procedure SetunidadesPostagem(Index: Integer; const AArray_Of_unidadePostagemERP: Array_Of_unidadePostagemERP);
    function  unidadesPostagem_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property bairro:           string                       Index (IS_OPTN or IS_UNQL) read Fbairro write Setbairro stored bairro_Specified;
    property cep:              string                       Index (IS_OPTN or IS_UNQL) read Fcep write Setcep stored cep_Specified;
    property cidade:           string                       Index (IS_OPTN or IS_UNQL) read Fcidade write Setcidade stored cidade_Specified;
    property complemento:      string                       Index (IS_OPTN or IS_UNQL) read Fcomplemento write Setcomplemento stored complemento_Specified;
    property complemento2:     string                       Index (IS_OPTN or IS_UNQL) read Fcomplemento2 write Setcomplemento2 stored complemento2_Specified;
    property end_:             string                       Index (IS_OPTN or IS_UNQL) read Fend_ write Setend_ stored end__Specified;
    property id:               Int64                        Index (IS_UNQL) read Fid write Fid;
    property uf:               string                       Index (IS_OPTN or IS_UNQL) read Fuf write Setuf stored uf_Specified;
    property unidadesPostagem: Array_Of_unidadePostagemERP  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FunidadesPostagem write SetunidadesPostagem stored unidadesPostagem_Specified;
  end;



  // ************************************************************************ //
  // XML       : contratoERPPK, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  contratoERPPK = class(TRemotable)
  private
    Fdiretoria: Int64;
    Fnumero: string;
    Fnumero_Specified: boolean;
    procedure Setnumero(Index: Integer; const Astring: string);
    function  numero_Specified(Index: Integer): boolean;
  published
    property diretoria: Int64   Index (IS_UNQL) read Fdiretoria write Fdiretoria;
    property numero:    string  Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
  end;



  // ************************************************************************ //
  // XML       : unidadePostagemERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  unidadePostagemERP = class(TRemotable)
  private
    FdiretoriaRegional: string;
    FdiretoriaRegional_Specified: boolean;
    Fendereco: enderecoERP;
    Fendereco_Specified: boolean;
    Fid: string;
    Fid_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    Fstatus: string;
    Fstatus_Specified: boolean;
    Ftipo: string;
    Ftipo_Specified: boolean;
    procedure SetdiretoriaRegional(Index: Integer; const Astring: string);
    function  diretoriaRegional_Specified(Index: Integer): boolean;
    procedure Setendereco(Index: Integer; const AenderecoERP: enderecoERP);
    function  endereco_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const Astring: string);
    function  id_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure Setstatus(Index: Integer; const Astring: string);
    function  status_Specified(Index: Integer): boolean;
    procedure Settipo(Index: Integer; const Astring: string);
    function  tipo_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property diretoriaRegional: string       Index (IS_OPTN or IS_UNQL) read FdiretoriaRegional write SetdiretoriaRegional stored diretoriaRegional_Specified;
    property endereco:          enderecoERP  Index (IS_OPTN or IS_UNQL) read Fendereco write Setendereco stored endereco_Specified;
    property id:                string       Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property nome:              string       Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property status:            string       Index (IS_OPTN or IS_UNQL) read Fstatus write Setstatus stored status_Specified;
    property tipo:              string       Index (IS_OPTN or IS_UNQL) read Ftipo write Settipo stored tipo_Specified;
  end;



  // ************************************************************************ //
  // XML       : servicoSigep, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  servicoSigep = class(TRemotable)
  private
    FcategoriaServico: categoriaServico;
    FcategoriaServico_Specified: boolean;
    Fchancela: chancelaMaster;
    Fchancela_Specified: boolean;
    FexigeDimensoes: Boolean;
    FexigeDimensoes_Specified: boolean;
    FexigeValorCobrar: Boolean;
    FexigeValorCobrar_Specified: boolean;
    Fimitm: Int64;
    Fservico: Int64;
    FservicoERP: servicoERP;
    FservicoERP_Specified: boolean;
    FssiCoCodigoPostal: string;
    FssiCoCodigoPostal_Specified: boolean;
    procedure SetcategoriaServico(Index: Integer; const AcategoriaServico: categoriaServico);
    function  categoriaServico_Specified(Index: Integer): boolean;
    procedure Setchancela(Index: Integer; const AchancelaMaster: chancelaMaster);
    function  chancela_Specified(Index: Integer): boolean;
    procedure SetexigeDimensoes(Index: Integer; const ABoolean: Boolean);
    function  exigeDimensoes_Specified(Index: Integer): boolean;
    procedure SetexigeValorCobrar(Index: Integer; const ABoolean: Boolean);
    function  exigeValorCobrar_Specified(Index: Integer): boolean;
    procedure SetservicoERP(Index: Integer; const AservicoERP: servicoERP);
    function  servicoERP_Specified(Index: Integer): boolean;
    procedure SetssiCoCodigoPostal(Index: Integer; const Astring: string);
    function  ssiCoCodigoPostal_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property categoriaServico:  categoriaServico  Index (IS_OPTN or IS_UNQL) read FcategoriaServico write SetcategoriaServico stored categoriaServico_Specified;
    property chancela:          chancelaMaster    Index (IS_OPTN or IS_UNQL) read Fchancela write Setchancela stored chancela_Specified;
    property exigeDimensoes:    Boolean           Index (IS_OPTN or IS_UNQL) read FexigeDimensoes write SetexigeDimensoes stored exigeDimensoes_Specified;
    property exigeValorCobrar:  Boolean           Index (IS_OPTN or IS_UNQL) read FexigeValorCobrar write SetexigeValorCobrar stored exigeValorCobrar_Specified;
    property imitm:             Int64             Index (IS_UNQL) read Fimitm write Fimitm;
    property servico:           Int64             Index (IS_UNQL) read Fservico write Fservico;
    property servicoERP:        servicoERP        Index (IS_OPTN or IS_UNQL) read FservicoERP write SetservicoERP stored servicoERP_Specified;
    property ssiCoCodigoPostal: string            Index (IS_OPTN or IS_UNQL) read FssiCoCodigoPostal write SetssiCoCodigoPostal stored ssiCoCodigoPostal_Specified;
  end;



  // ************************************************************************ //
  // XML       : SQLException, global, <element>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // Info      : Fault
  // Base Types: SQLException
  // ************************************************************************ //
  SQLException = class(ERemotableException)
  private
    FerrorCode: Integer;
    FerrorCode_Specified: boolean;
    FsQLState: string;
    FsQLState_Specified: boolean;
    Fmessage_: string;
    Fmessage__Specified: boolean;
    procedure SeterrorCode(Index: Integer; const AInteger: Integer);
    function  errorCode_Specified(Index: Integer): boolean;
    procedure SetsQLState(Index: Integer; const Astring: string);
    function  sQLState_Specified(Index: Integer): boolean;
    procedure Setmessage_(Index: Integer; const Astring: string);
    function  message__Specified(Index: Integer): boolean;
  published
    property errorCode: Integer  Index (IS_OPTN or IS_UNQL) read FerrorCode write SeterrorCode stored errorCode_Specified;
    property sQLState:  string   Index (IS_OPTN or IS_UNQL) read FsQLState write SetsQLState stored sQLState_Specified;
    property message_:  string   Index (IS_OPTN or IS_UNQL) read Fmessage_ write Setmessage_ stored message__Specified;
  end;



  // ************************************************************************ //
  // XML       : Exception, global, <element>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // Info      : Fault
  // Base Types: Exception
  // ************************************************************************ //
  Exception = class(ERemotableException)
  private
    Fmessage_: string;
    Fmessage__Specified: boolean;
    procedure Setmessage_(Index: Integer; const Astring: string);
    function  message__Specified(Index: Integer): boolean;
  published
    property message_: string  Index (IS_OPTN or IS_UNQL) read Fmessage_ write Setmessage_ stored message__Specified;
  end;



  // ************************************************************************ //
  // XML       : AutenticacaoException, alias
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // Serializtn: [xoSimpleTypeWrapper]
  // Info      : Fault
  // ************************************************************************ //
  AutenticacaoException = class(ERemotableException)
  private
    FValue: string;
  published
    property Value: string  read FValue write FValue;
  end;



  // ************************************************************************ //
  // XML       : SigepClienteException, alias
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // Serializtn: [xoSimpleTypeWrapper]
  // Info      : Fault
  // ************************************************************************ //
  SigepClienteException = class(ERemotableException)
  private
    FValue: string;
  published
    property Value: string  read FValue write FValue;
  end;

  Array_Of_string = array of string;            { "http://www.w3.org/2001/XMLSchema"[GblUbnd] }


  // ************************************************************************ //
  // XML       : pedidoInformacao, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  pedidoInformacao = class(TRemotable)
  private
    Fid: Int64;
    Fid_Specified: boolean;
    Fusuario: string;
    Fusuario_Specified: boolean;
    procedure Setid(Index: Integer; const AInt64: Int64);
    function  id_Specified(Index: Integer): boolean;
    procedure Setusuario(Index: Integer; const Astring: string);
    function  usuario_Specified(Index: Integer): boolean;
  published
    property id:      Int64   Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property usuario: string  Index (IS_OPTN or IS_UNQL) read Fusuario write Setusuario stored usuario_Specified;
  end;



  // ************************************************************************ //
  // XML       : pedidoInformacaoRegistro, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  pedidoInformacaoRegistro = class(pedidoInformacao)
  private
    Fcliente: cliente;
    Fcliente_Specified: boolean;
    FcodigoRegistro: string;
    FcodigoRegistro_Specified: boolean;
    Fconta: conta;
    Fconta_Specified: boolean;
    FconteudoObjeto: string;
    FconteudoObjeto_Specified: boolean;
    FcpfCnpj: string;
    FcpfCnpj_Specified: boolean;
    Fdestinatario: destinatario;
    Fdestinatario_Specified: boolean;
    Fembalagem: string;
    Fembalagem_Specified: boolean;
    Fmotivo: Integer;
    Fmotivo_Specified: boolean;
    Fobservacao: string;
    Fobservacao_Specified: boolean;
    Fpostagem: postagem;
    Fpostagem_Specified: boolean;
    Fremetente: remetente;
    Fremetente_Specified: boolean;
    Fservico: Integer;
    Fservico_Specified: boolean;
    FtipoDocumento: string;
    FtipoDocumento_Specified: boolean;
    procedure Setcliente(Index: Integer; const Acliente: cliente);
    function  cliente_Specified(Index: Integer): boolean;
    procedure SetcodigoRegistro(Index: Integer; const Astring: string);
    function  codigoRegistro_Specified(Index: Integer): boolean;
    procedure Setconta(Index: Integer; const Aconta: conta);
    function  conta_Specified(Index: Integer): boolean;
    procedure SetconteudoObjeto(Index: Integer; const Astring: string);
    function  conteudoObjeto_Specified(Index: Integer): boolean;
    procedure SetcpfCnpj(Index: Integer; const Astring: string);
    function  cpfCnpj_Specified(Index: Integer): boolean;
    procedure Setdestinatario(Index: Integer; const Adestinatario: destinatario);
    function  destinatario_Specified(Index: Integer): boolean;
    procedure Setembalagem(Index: Integer; const Astring: string);
    function  embalagem_Specified(Index: Integer): boolean;
    procedure Setmotivo(Index: Integer; const AInteger: Integer);
    function  motivo_Specified(Index: Integer): boolean;
    procedure Setobservacao(Index: Integer; const Astring: string);
    function  observacao_Specified(Index: Integer): boolean;
    procedure Setpostagem(Index: Integer; const Apostagem: postagem);
    function  postagem_Specified(Index: Integer): boolean;
    procedure Setremetente(Index: Integer; const Aremetente: remetente);
    function  remetente_Specified(Index: Integer): boolean;
    procedure Setservico(Index: Integer; const AInteger: Integer);
    function  servico_Specified(Index: Integer): boolean;
    procedure SettipoDocumento(Index: Integer; const Astring: string);
    function  tipoDocumento_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cliente:        cliente       Index (IS_OPTN or IS_UNQL) read Fcliente write Setcliente stored cliente_Specified;
    property codigoRegistro: string        Index (IS_OPTN or IS_UNQL) read FcodigoRegistro write SetcodigoRegistro stored codigoRegistro_Specified;
    property conta:          conta         Index (IS_OPTN or IS_UNQL) read Fconta write Setconta stored conta_Specified;
    property conteudoObjeto: string        Index (IS_OPTN or IS_UNQL) read FconteudoObjeto write SetconteudoObjeto stored conteudoObjeto_Specified;
    property cpfCnpj:        string        Index (IS_OPTN or IS_UNQL) read FcpfCnpj write SetcpfCnpj stored cpfCnpj_Specified;
    property destinatario:   destinatario  Index (IS_OPTN or IS_UNQL) read Fdestinatario write Setdestinatario stored destinatario_Specified;
    property embalagem:      string        Index (IS_OPTN or IS_UNQL) read Fembalagem write Setembalagem stored embalagem_Specified;
    property motivo:         Integer       Index (IS_OPTN or IS_UNQL) read Fmotivo write Setmotivo stored motivo_Specified;
    property observacao:     string        Index (IS_OPTN or IS_UNQL) read Fobservacao write Setobservacao stored observacao_Specified;
    property postagem:       postagem      Index (IS_OPTN or IS_UNQL) read Fpostagem write Setpostagem stored postagem_Specified;
    property remetente:      remetente     Index (IS_OPTN or IS_UNQL) read Fremetente write Setremetente stored remetente_Specified;
    property servico:        Integer       Index (IS_OPTN or IS_UNQL) read Fservico write Setservico stored servico_Specified;
    property tipoDocumento:  string        Index (IS_OPTN or IS_UNQL) read FtipoDocumento write SettipoDocumento stored tipoDocumento_Specified;
  end;



  // ************************************************************************ //
  // XML       : retorno, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  retorno = class(TRemotable)
  private
    FcodigoPI: Int64;
    FcodigoPI_Specified: boolean;
    FcodigoRegistro: string;
    FcodigoRegistro_Specified: boolean;
    FcodigoRetorno: string;
    FcodigoRetorno_Specified: boolean;
    FdataPrazoResposta: string;
    FdataPrazoResposta_Specified: boolean;
    FdataRegistro: string;
    FdataRegistro_Specified: boolean;
    FdataResposta: string;
    FdataResposta_Specified: boolean;
    FdataUltimaRecorrencia: string;
    FdataUltimaRecorrencia_Specified: boolean;
    Fid: Int64;
    Fid_Specified: boolean;
    FmensagemRetorno: string;
    FmensagemRetorno_Specified: boolean;
    Fresposta: string;
    Fresposta_Specified: boolean;
    procedure SetcodigoPI(Index: Integer; const AInt64: Int64);
    function  codigoPI_Specified(Index: Integer): boolean;
    procedure SetcodigoRegistro(Index: Integer; const Astring: string);
    function  codigoRegistro_Specified(Index: Integer): boolean;
    procedure SetcodigoRetorno(Index: Integer; const Astring: string);
    function  codigoRetorno_Specified(Index: Integer): boolean;
    procedure SetdataPrazoResposta(Index: Integer; const Astring: string);
    function  dataPrazoResposta_Specified(Index: Integer): boolean;
    procedure SetdataRegistro(Index: Integer; const Astring: string);
    function  dataRegistro_Specified(Index: Integer): boolean;
    procedure SetdataResposta(Index: Integer; const Astring: string);
    function  dataResposta_Specified(Index: Integer): boolean;
    procedure SetdataUltimaRecorrencia(Index: Integer; const Astring: string);
    function  dataUltimaRecorrencia_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const AInt64: Int64);
    function  id_Specified(Index: Integer): boolean;
    procedure SetmensagemRetorno(Index: Integer; const Astring: string);
    function  mensagemRetorno_Specified(Index: Integer): boolean;
    procedure Setresposta(Index: Integer; const Astring: string);
    function  resposta_Specified(Index: Integer): boolean;
  published
    property codigoPI:              Int64   Index (IS_OPTN or IS_UNQL) read FcodigoPI write SetcodigoPI stored codigoPI_Specified;
    property codigoRegistro:        string  Index (IS_OPTN or IS_UNQL) read FcodigoRegistro write SetcodigoRegistro stored codigoRegistro_Specified;
    property codigoRetorno:         string  Index (IS_OPTN or IS_UNQL) read FcodigoRetorno write SetcodigoRetorno stored codigoRetorno_Specified;
    property dataPrazoResposta:     string  Index (IS_OPTN or IS_UNQL) read FdataPrazoResposta write SetdataPrazoResposta stored dataPrazoResposta_Specified;
    property dataRegistro:          string  Index (IS_OPTN or IS_UNQL) read FdataRegistro write SetdataRegistro stored dataRegistro_Specified;
    property dataResposta:          string  Index (IS_OPTN or IS_UNQL) read FdataResposta write SetdataResposta stored dataResposta_Specified;
    property dataUltimaRecorrencia: string  Index (IS_OPTN or IS_UNQL) read FdataUltimaRecorrencia write SetdataUltimaRecorrencia stored dataUltimaRecorrencia_Specified;
    property id:                    Int64   Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property mensagemRetorno:       string  Index (IS_OPTN or IS_UNQL) read FmensagemRetorno write SetmensagemRetorno stored mensagemRetorno_Specified;
    property resposta:              string  Index (IS_OPTN or IS_UNQL) read Fresposta write Setresposta stored resposta_Specified;
  end;



  // ************************************************************************ //
  // XML       : clienteERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  clienteERP = class(TRemotable)
  private
    Fcnpj: string;
    Fcnpj_Specified: boolean;
    Fcontratos: Array_Of_contratoERP;
    Fcontratos_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdatajAtualizacao: Integer;
    FdatajAtualizacao_Specified: boolean;
    FdescricaoStatusCliente: string;
    FdescricaoStatusCliente_Specified: boolean;
    FgerenteConta: Array_Of_gerenteConta;
    FgerenteConta_Specified: boolean;
    FhorajAtualizacao: Int64;
    FhorajAtualizacao_Specified: boolean;
    Fid: Int64;
    FinscricaoEstadual: string;
    FinscricaoEstadual_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    FstatusCodigo: string;
    FstatusCodigo_Specified: boolean;
    procedure Setcnpj(Index: Integer; const Astring: string);
    function  cnpj_Specified(Index: Integer): boolean;
    procedure Setcontratos(Index: Integer; const AArray_Of_contratoERP: Array_Of_contratoERP);
    function  contratos_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
    function  datajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdescricaoStatusCliente(Index: Integer; const Astring: string);
    function  descricaoStatusCliente_Specified(Index: Integer): boolean;
    procedure SetgerenteConta(Index: Integer; const AArray_Of_gerenteConta: Array_Of_gerenteConta);
    function  gerenteConta_Specified(Index: Integer): boolean;
    procedure SethorajAtualizacao(Index: Integer; const AInt64: Int64);
    function  horajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetinscricaoEstadual(Index: Integer; const Astring: string);
    function  inscricaoEstadual_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure SetstatusCodigo(Index: Integer; const Astring: string);
    function  statusCodigo_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cnpj:                   string                 Index (IS_OPTN or IS_UNQL) read Fcnpj write Setcnpj stored cnpj_Specified;
    property contratos:              Array_Of_contratoERP   Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fcontratos write Setcontratos stored contratos_Specified;
    property dataAtualizacao:        TXSDateTime            Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property datajAtualizacao:       Integer                Index (IS_OPTN or IS_UNQL) read FdatajAtualizacao write SetdatajAtualizacao stored datajAtualizacao_Specified;
    property descricaoStatusCliente: string                 Index (IS_OPTN or IS_UNQL) read FdescricaoStatusCliente write SetdescricaoStatusCliente stored descricaoStatusCliente_Specified;
    property gerenteConta:           Array_Of_gerenteConta  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FgerenteConta write SetgerenteConta stored gerenteConta_Specified;
    property horajAtualizacao:       Int64                  Index (IS_OPTN or IS_UNQL) read FhorajAtualizacao write SethorajAtualizacao stored horajAtualizacao_Specified;
    property id:                     Int64                  Index (IS_UNQL) read Fid write Fid;
    property inscricaoEstadual:      string                 Index (IS_OPTN or IS_UNQL) read FinscricaoEstadual write SetinscricaoEstadual stored inscricaoEstadual_Specified;
    property nome:                   string                 Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property statusCodigo:           string                 Index (IS_OPTN or IS_UNQL) read FstatusCodigo write SetstatusCodigo stored statusCodigo_Specified;
  end;



  // ************************************************************************ //
  // XML       : chancelaMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  chancelaMaster = class(TRemotable)
  private
    Fchancela: TByteDynArray;
    Fchancela_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    Fid: Int64;
    FservicosSigep: Array_Of_servicoSigep;
    FservicosSigep_Specified: boolean;
    procedure Setchancela(Index: Integer; const ATByteDynArray: TByteDynArray);
    function  chancela_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
    procedure SetservicosSigep(Index: Integer; const AArray_Of_servicoSigep: Array_Of_servicoSigep);
    function  servicosSigep_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property chancela:        TByteDynArray          Index (IS_OPTN or IS_UNQL) read Fchancela write Setchancela stored chancela_Specified;
    property dataAtualizacao: TXSDateTime            Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property descricao:       string                 Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
    property id:              Int64                  Index (IS_UNQL) read Fid write Fid;
    property servicosSigep:   Array_Of_servicoSigep  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FservicosSigep write SetservicosSigep stored servicosSigep_Specified;
  end;



  // ************************************************************************ //
  // XML       : gerenteConta, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  gerenteConta = class(TRemotable)
  private
    FclientesVisiveis: Array_Of_clienteERP;
    FclientesVisiveis_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdataInclusao: TXSDateTime;
    FdataInclusao_Specified: boolean;
    FdataSenha: TXSDateTime;
    FdataSenha_Specified: boolean;
    Flogin: string;
    Flogin_Specified: boolean;
    Fmatricula: string;
    Fmatricula_Specified: boolean;
    Fsenha: string;
    Fsenha_Specified: boolean;
    Fstatus: statusGerente;
    Fstatus_Specified: boolean;
    FtipoGerente: tipoGerente;
    FtipoGerente_Specified: boolean;
    FusuariosInstalacao: Array_Of_usuarioInstalacao;
    FusuariosInstalacao_Specified: boolean;
    Fvalidade: string;
    Fvalidade_Specified: boolean;
    procedure SetclientesVisiveis(Index: Integer; const AArray_Of_clienteERP: Array_Of_clienteERP);
    function  clientesVisiveis_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataInclusao_Specified(Index: Integer): boolean;
    procedure SetdataSenha(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataSenha_Specified(Index: Integer): boolean;
    procedure Setlogin(Index: Integer; const Astring: string);
    function  login_Specified(Index: Integer): boolean;
    procedure Setmatricula(Index: Integer; const Astring: string);
    function  matricula_Specified(Index: Integer): boolean;
    procedure Setsenha(Index: Integer; const Astring: string);
    function  senha_Specified(Index: Integer): boolean;
    procedure Setstatus(Index: Integer; const AstatusGerente: statusGerente);
    function  status_Specified(Index: Integer): boolean;
    procedure SettipoGerente(Index: Integer; const AtipoGerente: tipoGerente);
    function  tipoGerente_Specified(Index: Integer): boolean;
    procedure SetusuariosInstalacao(Index: Integer; const AArray_Of_usuarioInstalacao: Array_Of_usuarioInstalacao);
    function  usuariosInstalacao_Specified(Index: Integer): boolean;
    procedure Setvalidade(Index: Integer; const Astring: string);
    function  validade_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property clientesVisiveis:   Array_Of_clienteERP         Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FclientesVisiveis write SetclientesVisiveis stored clientesVisiveis_Specified;
    property dataAtualizacao:    TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property dataInclusao:       TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataInclusao write SetdataInclusao stored dataInclusao_Specified;
    property dataSenha:          TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataSenha write SetdataSenha stored dataSenha_Specified;
    property login:              string                      Index (IS_OPTN or IS_UNQL) read Flogin write Setlogin stored login_Specified;
    property matricula:          string                      Index (IS_OPTN or IS_UNQL) read Fmatricula write Setmatricula stored matricula_Specified;
    property senha:              string                      Index (IS_OPTN or IS_UNQL) read Fsenha write Setsenha stored senha_Specified;
    property status:             statusGerente               Index (IS_OPTN or IS_UNQL) read Fstatus write Setstatus stored status_Specified;
    property tipoGerente:        tipoGerente                 Index (IS_OPTN or IS_UNQL) read FtipoGerente write SettipoGerente stored tipoGerente_Specified;
    property usuariosInstalacao: Array_Of_usuarioInstalacao  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FusuariosInstalacao write SetusuariosInstalacao stored usuariosInstalacao_Specified;
    property validade:           string                      Index (IS_OPTN or IS_UNQL) read Fvalidade write Setvalidade stored validade_Specified;
  end;



  // ************************************************************************ //
  // XML       : servicoAdicionalERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  servicoAdicionalERP = class(TRemotable)
  private
    Fcodigo: string;
    Fcodigo_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdatajAtualizacao: Integer;
    FdatajAtualizacao_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    FhorajAtualizacao: Integer;
    FhorajAtualizacao_Specified: boolean;
    Fid: Integer;
    Fid_Specified: boolean;
    Fsigla: string;
    Fsigla_Specified: boolean;
    procedure Setcodigo(Index: Integer; const Astring: string);
    function  codigo_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
    function  datajAtualizacao_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
    procedure SethorajAtualizacao(Index: Integer; const AInteger: Integer);
    function  horajAtualizacao_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const AInteger: Integer);
    function  id_Specified(Index: Integer): boolean;
    procedure Setsigla(Index: Integer; const Astring: string);
    function  sigla_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property codigo:           string       Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property dataAtualizacao:  TXSDateTime  Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property datajAtualizacao: Integer      Index (IS_OPTN or IS_UNQL) read FdatajAtualizacao write SetdatajAtualizacao stored datajAtualizacao_Specified;
    property descricao:        string       Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
    property horajAtualizacao: Integer      Index (IS_OPTN or IS_UNQL) read FhorajAtualizacao write SethorajAtualizacao stored horajAtualizacao_Specified;
    property id:               Integer      Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property sigla:            string       Index (IS_OPTN or IS_UNQL) read Fsigla write Setsigla stored sigla_Specified;
  end;



  // ************************************************************************ //
  // XML       : vigenciaERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  vigenciaERP = class(TRemotable)
  private
    FdataFinal: TXSDateTime;
    FdataFinal_Specified: boolean;
    FdataInicial: TXSDateTime;
    FdataInicial_Specified: boolean;
    FdatajFim: Integer;
    FdatajFim_Specified: boolean;
    FdatajIni: Integer;
    FdatajIni_Specified: boolean;
    Fid: Int64;
    Fid_Specified: boolean;
    procedure SetdataFinal(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataFinal_Specified(Index: Integer): boolean;
    procedure SetdataInicial(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataInicial_Specified(Index: Integer): boolean;
    procedure SetdatajFim(Index: Integer; const AInteger: Integer);
    function  datajFim_Specified(Index: Integer): boolean;
    procedure SetdatajIni(Index: Integer; const AInteger: Integer);
    function  datajIni_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const AInt64: Int64);
    function  id_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property dataFinal:   TXSDateTime  Index (IS_OPTN or IS_UNQL) read FdataFinal write SetdataFinal stored dataFinal_Specified;
    property dataInicial: TXSDateTime  Index (IS_OPTN or IS_UNQL) read FdataInicial write SetdataInicial stored dataInicial_Specified;
    property datajFim:    Integer      Index (IS_OPTN or IS_UNQL) read FdatajFim write SetdatajFim stored datajFim_Specified;
    property datajIni:    Integer      Index (IS_OPTN or IS_UNQL) read FdatajIni write SetdatajIni stored datajIni_Specified;
    property id:          Int64        Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
  end;



  // ************************************************************************ //
  // XML       : servicoERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  servicoERP = class(TRemotable)
  private
    Fcodigo: string;
    Fcodigo_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdatajAtualizacao: Integer;
    FdatajAtualizacao_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    FhorajAtualizacao: Integer;
    FhorajAtualizacao_Specified: boolean;
    Fid: Int64;
    FservicoSigep: servicoSigep;
    FservicoSigep_Specified: boolean;
    FservicosAdicionais: Array_Of_servicoAdicionalERP;
    FservicosAdicionais_Specified: boolean;
    Ftipo1Codigo: string;
    Ftipo1Codigo_Specified: boolean;
    Ftipo1Descricao: string;
    Ftipo1Descricao_Specified: boolean;
    Ftipo2Codigo: string;
    Ftipo2Codigo_Specified: boolean;
    Ftipo2Descricao: string;
    Ftipo2Descricao_Specified: boolean;
    Fvigencia: vigenciaERP;
    Fvigencia_Specified: boolean;
    procedure Setcodigo(Index: Integer; const Astring: string);
    function  codigo_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
    function  datajAtualizacao_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
    procedure SethorajAtualizacao(Index: Integer; const AInteger: Integer);
    function  horajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetservicoSigep(Index: Integer; const AservicoSigep: servicoSigep);
    function  servicoSigep_Specified(Index: Integer): boolean;
    procedure SetservicosAdicionais(Index: Integer; const AArray_Of_servicoAdicionalERP: Array_Of_servicoAdicionalERP);
    function  servicosAdicionais_Specified(Index: Integer): boolean;
    procedure Settipo1Codigo(Index: Integer; const Astring: string);
    function  tipo1Codigo_Specified(Index: Integer): boolean;
    procedure Settipo1Descricao(Index: Integer; const Astring: string);
    function  tipo1Descricao_Specified(Index: Integer): boolean;
    procedure Settipo2Codigo(Index: Integer; const Astring: string);
    function  tipo2Codigo_Specified(Index: Integer): boolean;
    procedure Settipo2Descricao(Index: Integer; const Astring: string);
    function  tipo2Descricao_Specified(Index: Integer): boolean;
    procedure Setvigencia(Index: Integer; const AvigenciaERP: vigenciaERP);
    function  vigencia_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property codigo:             string                        Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property dataAtualizacao:    TXSDateTime                   Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property datajAtualizacao:   Integer                       Index (IS_OPTN or IS_UNQL) read FdatajAtualizacao write SetdatajAtualizacao stored datajAtualizacao_Specified;
    property descricao:          string                        Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
    property horajAtualizacao:   Integer                       Index (IS_OPTN or IS_UNQL) read FhorajAtualizacao write SethorajAtualizacao stored horajAtualizacao_Specified;
    property id:                 Int64                         Index (IS_UNQL) read Fid write Fid;
    property servicoSigep:       servicoSigep                  Index (IS_OPTN or IS_UNQL) read FservicoSigep write SetservicoSigep stored servicoSigep_Specified;
    property servicosAdicionais: Array_Of_servicoAdicionalERP  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FservicosAdicionais write SetservicosAdicionais stored servicosAdicionais_Specified;
    property tipo1Codigo:        string                        Index (IS_OPTN or IS_UNQL) read Ftipo1Codigo write Settipo1Codigo stored tipo1Codigo_Specified;
    property tipo1Descricao:     string                        Index (IS_OPTN or IS_UNQL) read Ftipo1Descricao write Settipo1Descricao stored tipo1Descricao_Specified;
    property tipo2Codigo:        string                        Index (IS_OPTN or IS_UNQL) read Ftipo2Codigo write Settipo2Codigo stored tipo2Codigo_Specified;
    property tipo2Descricao:     string                        Index (IS_OPTN or IS_UNQL) read Ftipo2Descricao write Settipo2Descricao stored tipo2Descricao_Specified;
    property vigencia:           vigenciaERP                   Index (IS_OPTN or IS_UNQL) read Fvigencia write Setvigencia stored vigencia_Specified;
  end;



  // ************************************************************************ //
  // XML       : contratoERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  contratoERP = class(TRemotable)
  private
    FcartoesPostagem: Array_Of_cartaoPostagemERP;
    FcartoesPostagem_Specified: boolean;
    Fcliente: clienteERP;
    Fcliente_Specified: boolean;
    FcodigoCliente: Int64;
    FcodigoDiretoria: string;
    FcodigoDiretoria_Specified: boolean;
    FcontratoPK: contratoERPPK;
    FcontratoPK_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdataAtualizacaoDDMMYYYY: string;
    FdataAtualizacaoDDMMYYYY_Specified: boolean;
    FdataVigenciaFim: TXSDateTime;
    FdataVigenciaFim_Specified: boolean;
    FdataVigenciaFimDDMMYYYY: string;
    FdataVigenciaFimDDMMYYYY_Specified: boolean;
    FdataVigenciaInicio: TXSDateTime;
    FdataVigenciaInicio_Specified: boolean;
    FdataVigenciaInicioDDMMYYYY: string;
    FdataVigenciaInicioDDMMYYYY_Specified: boolean;
    FdatajAtualizacao: Integer;
    FdatajAtualizacao_Specified: boolean;
    FdatajVigenciaFim: Integer;
    FdatajVigenciaFim_Specified: boolean;
    FdatajVigenciaInicio: Integer;
    FdatajVigenciaInicio_Specified: boolean;
    FdescricaoDiretoriaRegional: string;
    FdescricaoDiretoriaRegional_Specified: boolean;
    FdescricaoStatus: string;
    FdescricaoStatus_Specified: boolean;
    FdiretoriaRegional: unidadePostagemERP;
    FdiretoriaRegional_Specified: boolean;
    FhorajAtualizacao: Integer;
    FhorajAtualizacao_Specified: boolean;
    FstatusCodigo: string;
    FstatusCodigo_Specified: boolean;
    procedure SetcartoesPostagem(Index: Integer; const AArray_Of_cartaoPostagemERP: Array_Of_cartaoPostagemERP);
    function  cartoesPostagem_Specified(Index: Integer): boolean;
    procedure Setcliente(Index: Integer; const AclienteERP: clienteERP);
    function  cliente_Specified(Index: Integer): boolean;
    procedure SetcodigoDiretoria(Index: Integer; const Astring: string);
    function  codigoDiretoria_Specified(Index: Integer): boolean;
    procedure SetcontratoPK(Index: Integer; const AcontratoERPPK: contratoERPPK);
    function  contratoPK_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacaoDDMMYYYY(Index: Integer; const Astring: string);
    function  dataAtualizacaoDDMMYYYY_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaFim(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataVigenciaFim_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaFimDDMMYYYY(Index: Integer; const Astring: string);
    function  dataVigenciaFimDDMMYYYY_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaInicio(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataVigenciaInicio_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaInicioDDMMYYYY(Index: Integer; const Astring: string);
    function  dataVigenciaInicioDDMMYYYY_Specified(Index: Integer): boolean;
    procedure SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
    function  datajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdatajVigenciaFim(Index: Integer; const AInteger: Integer);
    function  datajVigenciaFim_Specified(Index: Integer): boolean;
    procedure SetdatajVigenciaInicio(Index: Integer; const AInteger: Integer);
    function  datajVigenciaInicio_Specified(Index: Integer): boolean;
    procedure SetdescricaoDiretoriaRegional(Index: Integer; const Astring: string);
    function  descricaoDiretoriaRegional_Specified(Index: Integer): boolean;
    procedure SetdescricaoStatus(Index: Integer; const Astring: string);
    function  descricaoStatus_Specified(Index: Integer): boolean;
    procedure SetdiretoriaRegional(Index: Integer; const AunidadePostagemERP: unidadePostagemERP);
    function  diretoriaRegional_Specified(Index: Integer): boolean;
    procedure SethorajAtualizacao(Index: Integer; const AInteger: Integer);
    function  horajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetstatusCodigo(Index: Integer; const Astring: string);
    function  statusCodigo_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cartoesPostagem:            Array_Of_cartaoPostagemERP  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FcartoesPostagem write SetcartoesPostagem stored cartoesPostagem_Specified;
    property cliente:                    clienteERP                  Index (IS_OPTN or IS_UNQL) read Fcliente write Setcliente stored cliente_Specified;
    property codigoCliente:              Int64                       Index (IS_UNQL) read FcodigoCliente write FcodigoCliente;
    property codigoDiretoria:            string                      Index (IS_OPTN or IS_UNQL) read FcodigoDiretoria write SetcodigoDiretoria stored codigoDiretoria_Specified;
    property contratoPK:                 contratoERPPK               Index (IS_OPTN or IS_UNQL) read FcontratoPK write SetcontratoPK stored contratoPK_Specified;
    property dataAtualizacao:            TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property dataAtualizacaoDDMMYYYY:    string                      Index (IS_OPTN or IS_UNQL) read FdataAtualizacaoDDMMYYYY write SetdataAtualizacaoDDMMYYYY stored dataAtualizacaoDDMMYYYY_Specified;
    property dataVigenciaFim:            TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataVigenciaFim write SetdataVigenciaFim stored dataVigenciaFim_Specified;
    property dataVigenciaFimDDMMYYYY:    string                      Index (IS_OPTN or IS_UNQL) read FdataVigenciaFimDDMMYYYY write SetdataVigenciaFimDDMMYYYY stored dataVigenciaFimDDMMYYYY_Specified;
    property dataVigenciaInicio:         TXSDateTime                 Index (IS_OPTN or IS_UNQL) read FdataVigenciaInicio write SetdataVigenciaInicio stored dataVigenciaInicio_Specified;
    property dataVigenciaInicioDDMMYYYY: string                      Index (IS_OPTN or IS_UNQL) read FdataVigenciaInicioDDMMYYYY write SetdataVigenciaInicioDDMMYYYY stored dataVigenciaInicioDDMMYYYY_Specified;
    property datajAtualizacao:           Integer                     Index (IS_OPTN or IS_UNQL) read FdatajAtualizacao write SetdatajAtualizacao stored datajAtualizacao_Specified;
    property datajVigenciaFim:           Integer                     Index (IS_OPTN or IS_UNQL) read FdatajVigenciaFim write SetdatajVigenciaFim stored datajVigenciaFim_Specified;
    property datajVigenciaInicio:        Integer                     Index (IS_OPTN or IS_UNQL) read FdatajVigenciaInicio write SetdatajVigenciaInicio stored datajVigenciaInicio_Specified;
    property descricaoDiretoriaRegional: string                      Index (IS_OPTN or IS_UNQL) read FdescricaoDiretoriaRegional write SetdescricaoDiretoriaRegional stored descricaoDiretoriaRegional_Specified;
    property descricaoStatus:            string                      Index (IS_OPTN or IS_UNQL) read FdescricaoStatus write SetdescricaoStatus stored descricaoStatus_Specified;
    property diretoriaRegional:          unidadePostagemERP          Index (IS_OPTN or IS_UNQL) read FdiretoriaRegional write SetdiretoriaRegional stored diretoriaRegional_Specified;
    property horajAtualizacao:           Integer                     Index (IS_OPTN or IS_UNQL) read FhorajAtualizacao write SethorajAtualizacao stored horajAtualizacao_Specified;
    property statusCodigo:               string                      Index (IS_OPTN or IS_UNQL) read FstatusCodigo write SetstatusCodigo stored statusCodigo_Specified;
  end;



  // ************************************************************************ //
  // XML       : postagem, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  postagem = class(TRemotable)
  private
    Fagencia: string;
    Fagencia_Specified: boolean;
    FavisoRecebimento: string;
    FavisoRecebimento_Specified: boolean;
    Fdata: string;
    Fdata_Specified: boolean;
    Flocal_: string;
    Flocal__Specified: boolean;
    FvalorDeclarado: string;
    FvalorDeclarado_Specified: boolean;
    procedure Setagencia(Index: Integer; const Astring: string);
    function  agencia_Specified(Index: Integer): boolean;
    procedure SetavisoRecebimento(Index: Integer; const Astring: string);
    function  avisoRecebimento_Specified(Index: Integer): boolean;
    procedure Setdata(Index: Integer; const Astring: string);
    function  data_Specified(Index: Integer): boolean;
    procedure Setlocal_(Index: Integer; const Astring: string);
    function  local__Specified(Index: Integer): boolean;
    procedure SetvalorDeclarado(Index: Integer; const Astring: string);
    function  valorDeclarado_Specified(Index: Integer): boolean;
  published
    property agencia:          string  Index (IS_OPTN or IS_UNQL) read Fagencia write Setagencia stored agencia_Specified;
    property avisoRecebimento: string  Index (IS_OPTN or IS_UNQL) read FavisoRecebimento write SetavisoRecebimento stored avisoRecebimento_Specified;
    property data:             string  Index (IS_OPTN or IS_UNQL) read Fdata write Setdata stored data_Specified;
    property local_:           string  Index (IS_OPTN or IS_UNQL) read Flocal_ write Setlocal_ stored local__Specified;
    property valorDeclarado:   string  Index (IS_OPTN or IS_UNQL) read FvalorDeclarado write SetvalorDeclarado stored valorDeclarado_Specified;
  end;

  registrarPedidosInformacaoResponse = array of retorno;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : pessoa, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  pessoa = class(TRemotable)
  private
    Fbairro: string;
    Fbairro_Specified: boolean;
    Fcep: string;
    Fcep_Specified: boolean;
    Fcidade: string;
    Fcidade_Specified: boolean;
    Fcomplemento: string;
    Fcomplemento_Specified: boolean;
    Fddd: string;
    Fddd_Specified: boolean;
    Fendereco: string;
    Fendereco_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    Fnumero: string;
    Fnumero_Specified: boolean;
    Fpais: string;
    Fpais_Specified: boolean;
    Ftelefone: string;
    Ftelefone_Specified: boolean;
    Fuf: string;
    Fuf_Specified: boolean;
    procedure Setbairro(Index: Integer; const Astring: string);
    function  bairro_Specified(Index: Integer): boolean;
    procedure Setcep(Index: Integer; const Astring: string);
    function  cep_Specified(Index: Integer): boolean;
    procedure Setcidade(Index: Integer; const Astring: string);
    function  cidade_Specified(Index: Integer): boolean;
    procedure Setcomplemento(Index: Integer; const Astring: string);
    function  complemento_Specified(Index: Integer): boolean;
    procedure Setddd(Index: Integer; const Astring: string);
    function  ddd_Specified(Index: Integer): boolean;
    procedure Setendereco(Index: Integer; const Astring: string);
    function  endereco_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure Setnumero(Index: Integer; const Astring: string);
    function  numero_Specified(Index: Integer): boolean;
    procedure Setpais(Index: Integer; const Astring: string);
    function  pais_Specified(Index: Integer): boolean;
    procedure Settelefone(Index: Integer; const Astring: string);
    function  telefone_Specified(Index: Integer): boolean;
    procedure Setuf(Index: Integer; const Astring: string);
    function  uf_Specified(Index: Integer): boolean;
  published
    property bairro:      string  Index (IS_OPTN or IS_UNQL) read Fbairro write Setbairro stored bairro_Specified;
    property cep:         string  Index (IS_OPTN or IS_UNQL) read Fcep write Setcep stored cep_Specified;
    property cidade:      string  Index (IS_OPTN or IS_UNQL) read Fcidade write Setcidade stored cidade_Specified;
    property complemento: string  Index (IS_OPTN or IS_UNQL) read Fcomplemento write Setcomplemento stored complemento_Specified;
    property ddd:         string  Index (IS_OPTN or IS_UNQL) read Fddd write Setddd stored ddd_Specified;
    property endereco:    string  Index (IS_OPTN or IS_UNQL) read Fendereco write Setendereco stored endereco_Specified;
    property nome:        string  Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property numero:      string  Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
    property pais:        string  Index (IS_OPTN or IS_UNQL) read Fpais write Setpais stored pais_Specified;
    property telefone:    string  Index (IS_OPTN or IS_UNQL) read Ftelefone write Settelefone stored telefone_Specified;
    property uf:          string  Index (IS_OPTN or IS_UNQL) read Fuf write Setuf stored uf_Specified;
  end;



  // ************************************************************************ //
  // XML       : remetente, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  remetente = class(pessoa)
  private
    Femail: string;
    Femail_Specified: boolean;
    Fempresa: string;
    Fempresa_Specified: boolean;
    Ffax: string;
    Ffax_Specified: boolean;
    procedure Setemail(Index: Integer; const Astring: string);
    function  email_Specified(Index: Integer): boolean;
    procedure Setempresa(Index: Integer; const Astring: string);
    function  empresa_Specified(Index: Integer): boolean;
    procedure Setfax(Index: Integer; const Astring: string);
    function  fax_Specified(Index: Integer): boolean;
  published
    property email:   string  Index (IS_OPTN or IS_UNQL) read Femail write Setemail stored email_Specified;
    property empresa: string  Index (IS_OPTN or IS_UNQL) read Fempresa write Setempresa stored empresa_Specified;
    property fax:     string  Index (IS_OPTN or IS_UNQL) read Ffax write Setfax stored fax_Specified;
  end;



  // ************************************************************************ //
  // XML       : destinatario, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  destinatario = class(pessoa)
  private
  published
  end;



  // ************************************************************************ //
  // XML       : parametroMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  parametroMaster = class(TRemotable)
  private
    FprmCoParametro: Int64;
    FprmTxParametro: string;
    FprmTxParametro_Specified: boolean;
    FprmTxValor: string;
    FprmTxValor_Specified: boolean;
    procedure SetprmTxParametro(Index: Integer; const Astring: string);
    function  prmTxParametro_Specified(Index: Integer): boolean;
    procedure SetprmTxValor(Index: Integer; const Astring: string);
    function  prmTxValor_Specified(Index: Integer): boolean;
  published
    property prmCoParametro: Int64   Index (IS_UNQL) read FprmCoParametro write FprmCoParametro;
    property prmTxParametro: string  Index (IS_OPTN or IS_UNQL) read FprmTxParametro write SetprmTxParametro stored prmTxParametro_Specified;
    property prmTxValor:     string  Index (IS_OPTN or IS_UNQL) read FprmTxValor write SetprmTxValor stored prmTxValor_Specified;
  end;



  // ************************************************************************ //
  // XML       : embalagemLRSMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  embalagemLRSMaster = class(TRemotable)
  private
    Fcodigo: string;
    Fcodigo_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    Ftipo: string;
    Ftipo_Specified: boolean;
    procedure Setcodigo(Index: Integer; const Astring: string);
    function  codigo_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure Settipo(Index: Integer; const Astring: string);
    function  tipo_Specified(Index: Integer): boolean;
  published
    property codigo: string  Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property nome:   string  Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property tipo:   string  Index (IS_OPTN or IS_UNQL) read Ftipo write Settipo stored tipo_Specified;
  end;

  obterEmbalagemLRSResponse = array of embalagemLRSMaster;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : objetoSimplificadoTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  objetoSimplificadoTO = class(TRemotable)
  private
    Fdatahora_cancelamento: string;
    Fdatahora_cancelamento_Specified: boolean;
    Fnumero_pedido: Integer;
    Fnumero_pedido_Specified: boolean;
    Fstatus_pedido: string;
    Fstatus_pedido_Specified: boolean;
    procedure Setdatahora_cancelamento(Index: Integer; const Astring: string);
    function  datahora_cancelamento_Specified(Index: Integer): boolean;
    procedure Setnumero_pedido(Index: Integer; const AInteger: Integer);
    function  numero_pedido_Specified(Index: Integer): boolean;
    procedure Setstatus_pedido(Index: Integer; const Astring: string);
    function  status_pedido_Specified(Index: Integer): boolean;
  published
    property datahora_cancelamento: string   Index (IS_OPTN or IS_UNQL) read Fdatahora_cancelamento write Setdatahora_cancelamento stored datahora_cancelamento_Specified;
    property numero_pedido:         Integer  Index (IS_OPTN or IS_UNQL) read Fnumero_pedido write Setnumero_pedido stored numero_pedido_Specified;
    property status_pedido:         string   Index (IS_OPTN or IS_UNQL) read Fstatus_pedido write Setstatus_pedido stored status_pedido_Specified;
  end;



  // ************************************************************************ //
  // XML       : retornoCancelamentoTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  retornoCancelamentoTO = class(TRemotable)
  private
    Fcod_erro: string;
    Fcod_erro_Specified: boolean;
    Fcodigo_administrativo: string;
    Fcodigo_administrativo_Specified: boolean;
    Fdata: string;
    Fdata_Specified: boolean;
    Fhora: string;
    Fhora_Specified: boolean;
    Fmsg_erro: string;
    Fmsg_erro_Specified: boolean;
    Fobjeto_postal: objetoSimplificadoTO;
    Fobjeto_postal_Specified: boolean;
    procedure Setcod_erro(Index: Integer; const Astring: string);
    function  cod_erro_Specified(Index: Integer): boolean;
    procedure Setcodigo_administrativo(Index: Integer; const Astring: string);
    function  codigo_administrativo_Specified(Index: Integer): boolean;
    procedure Setdata(Index: Integer; const Astring: string);
    function  data_Specified(Index: Integer): boolean;
    procedure Sethora(Index: Integer; const Astring: string);
    function  hora_Specified(Index: Integer): boolean;
    procedure Setmsg_erro(Index: Integer; const Astring: string);
    function  msg_erro_Specified(Index: Integer): boolean;
    procedure Setobjeto_postal(Index: Integer; const AobjetoSimplificadoTO: objetoSimplificadoTO);
    function  objeto_postal_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cod_erro:              string                Index (IS_OPTN or IS_UNQL) read Fcod_erro write Setcod_erro stored cod_erro_Specified;
    property codigo_administrativo: string                Index (IS_OPTN or IS_UNQL) read Fcodigo_administrativo write Setcodigo_administrativo stored codigo_administrativo_Specified;
    property data:                  string                Index (IS_OPTN or IS_UNQL) read Fdata write Setdata stored data_Specified;
    property hora:                  string                Index (IS_OPTN or IS_UNQL) read Fhora write Sethora stored hora_Specified;
    property msg_erro:              string                Index (IS_OPTN or IS_UNQL) read Fmsg_erro write Setmsg_erro stored msg_erro_Specified;
    property objeto_postal:         objetoSimplificadoTO  Index (IS_OPTN or IS_UNQL) read Fobjeto_postal write Setobjeto_postal stored objeto_postal_Specified;
  end;



  // ************************************************************************ //
  // XML       : produtoTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  produtoTO = class(TRemotable)
  private
    Fcodigo: Int64;
    Fcodigo_Specified: boolean;
    Fqtd: Int64;
    Fqtd_Specified: boolean;
    Ftipo: Int64;
    Ftipo_Specified: boolean;
    procedure Setcodigo(Index: Integer; const AInt64: Int64);
    function  codigo_Specified(Index: Integer): boolean;
    procedure Setqtd(Index: Integer; const AInt64: Int64);
    function  qtd_Specified(Index: Integer): boolean;
    procedure Settipo(Index: Integer; const AInt64: Int64);
    function  tipo_Specified(Index: Integer): boolean;
  published
    property codigo: Int64  Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property qtd:    Int64  Index (IS_OPTN or IS_UNQL) read Fqtd write Setqtd stored qtd_Specified;
    property tipo:   Int64  Index (IS_OPTN or IS_UNQL) read Ftipo write Settipo stored tipo_Specified;
  end;



  // ************************************************************************ //
  // XML       : pessoaTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  pessoaTO = class(TRemotable)
  private
    Fbairro: string;
    Fbairro_Specified: boolean;
    Fcep: string;
    Fcep_Specified: boolean;
    Fcidade: string;
    Fcidade_Specified: boolean;
    Fcomplemento: string;
    Fcomplemento_Specified: boolean;
    Fddd: string;
    Fddd_Specified: boolean;
    Femail: string;
    Femail_Specified: boolean;
    Flogradouro: string;
    Flogradouro_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    Fnumero: string;
    Fnumero_Specified: boolean;
    Freferencia: string;
    Freferencia_Specified: boolean;
    Ftelefone: string;
    Ftelefone_Specified: boolean;
    Fuf: string;
    Fuf_Specified: boolean;
    procedure Setbairro(Index: Integer; const Astring: string);
    function  bairro_Specified(Index: Integer): boolean;
    procedure Setcep(Index: Integer; const Astring: string);
    function  cep_Specified(Index: Integer): boolean;
    procedure Setcidade(Index: Integer; const Astring: string);
    function  cidade_Specified(Index: Integer): boolean;
    procedure Setcomplemento(Index: Integer; const Astring: string);
    function  complemento_Specified(Index: Integer): boolean;
    procedure Setddd(Index: Integer; const Astring: string);
    function  ddd_Specified(Index: Integer): boolean;
    procedure Setemail(Index: Integer; const Astring: string);
    function  email_Specified(Index: Integer): boolean;
    procedure Setlogradouro(Index: Integer; const Astring: string);
    function  logradouro_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure Setnumero(Index: Integer; const Astring: string);
    function  numero_Specified(Index: Integer): boolean;
    procedure Setreferencia(Index: Integer; const Astring: string);
    function  referencia_Specified(Index: Integer): boolean;
    procedure Settelefone(Index: Integer; const Astring: string);
    function  telefone_Specified(Index: Integer): boolean;
    procedure Setuf(Index: Integer; const Astring: string);
    function  uf_Specified(Index: Integer): boolean;
  published
    property bairro:      string  Index (IS_OPTN or IS_UNQL) read Fbairro write Setbairro stored bairro_Specified;
    property cep:         string  Index (IS_OPTN or IS_UNQL) read Fcep write Setcep stored cep_Specified;
    property cidade:      string  Index (IS_OPTN or IS_UNQL) read Fcidade write Setcidade stored cidade_Specified;
    property complemento: string  Index (IS_OPTN or IS_UNQL) read Fcomplemento write Setcomplemento stored complemento_Specified;
    property ddd:         string  Index (IS_OPTN or IS_UNQL) read Fddd write Setddd stored ddd_Specified;
    property email:       string  Index (IS_OPTN or IS_UNQL) read Femail write Setemail stored email_Specified;
    property logradouro:  string  Index (IS_OPTN or IS_UNQL) read Flogradouro write Setlogradouro stored logradouro_Specified;
    property nome:        string  Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property numero:      string  Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
    property referencia:  string  Index (IS_OPTN or IS_UNQL) read Freferencia write Setreferencia stored referencia_Specified;
    property telefone:    string  Index (IS_OPTN or IS_UNQL) read Ftelefone write Settelefone stored telefone_Specified;
    property uf:          string  Index (IS_OPTN or IS_UNQL) read Fuf write Setuf stored uf_Specified;
  end;



  // ************************************************************************ //
  // XML       : remetenteTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  remetenteTO = class(pessoaTO)
  private
    Fidentificacao: string;
    Fidentificacao_Specified: boolean;
    procedure Setidentificacao(Index: Integer; const Astring: string);
    function  identificacao_Specified(Index: Integer): boolean;
  published
    property identificacao: string  Index (IS_OPTN or IS_UNQL) read Fidentificacao write Setidentificacao stored identificacao_Specified;
  end;



  // ************************************************************************ //
  // XML       : assuntoPIMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  assuntoPIMaster = class(TRemotable)
  private
    Fcodigo: Integer;
    Fcodigo_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    Ftipo: string;
    Ftipo_Specified: boolean;
    procedure Setcodigo(Index: Integer; const AInteger: Integer);
    function  codigo_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
    procedure Settipo(Index: Integer; const Astring: string);
    function  tipo_Specified(Index: Integer): boolean;
  published
    property codigo:    Integer  Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property descricao: string   Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
    property tipo:      string   Index (IS_OPTN or IS_UNQL) read Ftipo write Settipo stored tipo_Specified;
  end;

  obterAssuntosPIResponse = array of assuntoPIMaster;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : SQLException, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  SQLException2 = class(TRemotable)
  private
    FerrorCode: Integer;
    FerrorCode_Specified: boolean;
    FsQLState: string;
    FsQLState_Specified: boolean;
    Fmessage_: string;
    Fmessage__Specified: boolean;
    procedure SeterrorCode(Index: Integer; const AInteger: Integer);
    function  errorCode_Specified(Index: Integer): boolean;
    procedure SetsQLState(Index: Integer; const Astring: string);
    function  sQLState_Specified(Index: Integer): boolean;
    procedure Setmessage_(Index: Integer; const Astring: string);
    function  message__Specified(Index: Integer): boolean;
  published
    property errorCode: Integer  Index (IS_OPTN or IS_UNQL) read FerrorCode write SeterrorCode stored errorCode_Specified;
    property sQLState:  string   Index (IS_OPTN or IS_UNQL) read FsQLState write SetsQLState stored sQLState_Specified;
    property message_:  string   Index (IS_OPTN or IS_UNQL) read Fmessage_ write Setmessage_ stored message__Specified;
  end;



  // ************************************************************************ //
  // XML       : Exception, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  Exception2 = class(TRemotable)
  private
    Fmessage_: string;
    Fmessage__Specified: boolean;
    procedure Setmessage_(Index: Integer; const Astring: string);
    function  message__Specified(Index: Integer): boolean;
  published
    property message_: string  Index (IS_OPTN or IS_UNQL) read Fmessage_ write Setmessage_ stored message__Specified;
  end;

  buscaServicosResponse = array of servicoERP;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : cartaoPostagemERP, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  cartaoPostagemERP = class(TRemotable)
  private
    FcodigoAdministrativo: string;
    FcodigoAdministrativo_Specified: boolean;
    Fcontratos: Array_Of_contratoERP;
    Fcontratos_Specified: boolean;
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdataVigenciaFim: TXSDateTime;
    FdataVigenciaFim_Specified: boolean;
    FdataVigenciaInicio: TXSDateTime;
    FdataVigenciaInicio_Specified: boolean;
    FdatajAtualizacao: Integer;
    FdatajAtualizacao_Specified: boolean;
    FdatajVigenciaFim: Integer;
    FdatajVigenciaFim_Specified: boolean;
    FdatajVigenciaInicio: Integer;
    FdatajVigenciaInicio_Specified: boolean;
    FdescricaoStatusCartao: string;
    FdescricaoStatusCartao_Specified: boolean;
    FdescricaoUnidadePostagemGenerica: string;
    FdescricaoUnidadePostagemGenerica_Specified: boolean;
    FhorajAtualizacao: Integer;
    FhorajAtualizacao_Specified: boolean;
    Fnumero: string;
    Fnumero_Specified: boolean;
    Fservicos: buscaServicosResponse;
    Fservicos_Specified: boolean;
    FstatusCartaoPostagem: string;
    FstatusCartaoPostagem_Specified: boolean;
    FstatusCodigo: string;
    FstatusCodigo_Specified: boolean;
    FunidadeGenerica: string;
    FunidadeGenerica_Specified: boolean;
    FunidadesPostagem: Array_Of_unidadePostagemERP;
    FunidadesPostagem_Specified: boolean;
    procedure SetcodigoAdministrativo(Index: Integer; const Astring: string);
    function  codigoAdministrativo_Specified(Index: Integer): boolean;
    procedure Setcontratos(Index: Integer; const AArray_Of_contratoERP: Array_Of_contratoERP);
    function  contratos_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaFim(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataVigenciaFim_Specified(Index: Integer): boolean;
    procedure SetdataVigenciaInicio(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataVigenciaInicio_Specified(Index: Integer): boolean;
    procedure SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
    function  datajAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdatajVigenciaFim(Index: Integer; const AInteger: Integer);
    function  datajVigenciaFim_Specified(Index: Integer): boolean;
    procedure SetdatajVigenciaInicio(Index: Integer; const AInteger: Integer);
    function  datajVigenciaInicio_Specified(Index: Integer): boolean;
    procedure SetdescricaoStatusCartao(Index: Integer; const Astring: string);
    function  descricaoStatusCartao_Specified(Index: Integer): boolean;
    procedure SetdescricaoUnidadePostagemGenerica(Index: Integer; const Astring: string);
    function  descricaoUnidadePostagemGenerica_Specified(Index: Integer): boolean;
    procedure SethorajAtualizacao(Index: Integer; const AInteger: Integer);
    function  horajAtualizacao_Specified(Index: Integer): boolean;
    procedure Setnumero(Index: Integer; const Astring: string);
    function  numero_Specified(Index: Integer): boolean;
    procedure Setservicos(Index: Integer; const AbuscaServicosResponse: buscaServicosResponse);
    function  servicos_Specified(Index: Integer): boolean;
    procedure SetstatusCartaoPostagem(Index: Integer; const Astring: string);
    function  statusCartaoPostagem_Specified(Index: Integer): boolean;
    procedure SetstatusCodigo(Index: Integer; const Astring: string);
    function  statusCodigo_Specified(Index: Integer): boolean;
    procedure SetunidadeGenerica(Index: Integer; const Astring: string);
    function  unidadeGenerica_Specified(Index: Integer): boolean;
    procedure SetunidadesPostagem(Index: Integer; const AArray_Of_unidadePostagemERP: Array_Of_unidadePostagemERP);
    function  unidadesPostagem_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property codigoAdministrativo:             string                       Index (IS_OPTN or IS_UNQL) read FcodigoAdministrativo write SetcodigoAdministrativo stored codigoAdministrativo_Specified;
    property contratos:                        Array_Of_contratoERP         Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fcontratos write Setcontratos stored contratos_Specified;
    property dataAtualizacao:                  TXSDateTime                  Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property dataVigenciaFim:                  TXSDateTime                  Index (IS_OPTN or IS_UNQL) read FdataVigenciaFim write SetdataVigenciaFim stored dataVigenciaFim_Specified;
    property dataVigenciaInicio:               TXSDateTime                  Index (IS_OPTN or IS_UNQL) read FdataVigenciaInicio write SetdataVigenciaInicio stored dataVigenciaInicio_Specified;
    property datajAtualizacao:                 Integer                      Index (IS_OPTN or IS_UNQL) read FdatajAtualizacao write SetdatajAtualizacao stored datajAtualizacao_Specified;
    property datajVigenciaFim:                 Integer                      Index (IS_OPTN or IS_UNQL) read FdatajVigenciaFim write SetdatajVigenciaFim stored datajVigenciaFim_Specified;
    property datajVigenciaInicio:              Integer                      Index (IS_OPTN or IS_UNQL) read FdatajVigenciaInicio write SetdatajVigenciaInicio stored datajVigenciaInicio_Specified;
    property descricaoStatusCartao:            string                       Index (IS_OPTN or IS_UNQL) read FdescricaoStatusCartao write SetdescricaoStatusCartao stored descricaoStatusCartao_Specified;
    property descricaoUnidadePostagemGenerica: string                       Index (IS_OPTN or IS_UNQL) read FdescricaoUnidadePostagemGenerica write SetdescricaoUnidadePostagemGenerica stored descricaoUnidadePostagemGenerica_Specified;
    property horajAtualizacao:                 Integer                      Index (IS_OPTN or IS_UNQL) read FhorajAtualizacao write SethorajAtualizacao stored horajAtualizacao_Specified;
    property numero:                           string                       Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
    property servicos:                         buscaServicosResponse        Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fservicos write Setservicos stored servicos_Specified;
    property statusCartaoPostagem:             string                       Index (IS_OPTN or IS_UNQL) read FstatusCartaoPostagem write SetstatusCartaoPostagem stored statusCartaoPostagem_Specified;
    property statusCodigo:                     string                       Index (IS_OPTN or IS_UNQL) read FstatusCodigo write SetstatusCodigo stored statusCodigo_Specified;
    property unidadeGenerica:                  string                       Index (IS_OPTN or IS_UNQL) read FunidadeGenerica write SetunidadeGenerica stored unidadeGenerica_Specified;
    property unidadesPostagem:                 Array_Of_unidadePostagemERP  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FunidadesPostagem write SetunidadesPostagem stored unidadesPostagem_Specified;
  end;



  // ************************************************************************ //
  // XML       : motivoPIMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  motivoPIMaster = class(TRemotable)
  private
    Fcodigo: Integer;
    Fcodigo_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    procedure Setcodigo(Index: Integer; const AInteger: Integer);
    function  codigo_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
  published
    property codigo:    Integer  Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property descricao: string   Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
  end;

  obterMotivosPIResponse = array of motivoPIMaster;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }
  Array_Of_unsignedShort = array of Word;       { "http://www.w3.org/2001/XMLSchema"[GblUbnd] }


  // ************************************************************************ //
  // XML       : objetoPostal, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  objetoPostal = class(TRemotable)
  private
    FcodigoEtiqueta: string;
    FcodigoEtiqueta_Specified: boolean;
    FdataAtualizacaoCliente: TXSDateTime;
    FdataAtualizacaoCliente_Specified: boolean;
    FdataCancelamentoEtiqueta: TXSDateTime;
    FdataCancelamentoEtiqueta_Specified: boolean;
    FdataInclusao: TXSDateTime;
    FdataInclusao_Specified: boolean;
    FobjetoPostalPK: objetoPostalPK;
    FobjetoPostalPK_Specified: boolean;
    FplpNu: Int64;
    FplpNu_Specified: boolean;
    FpreListaPostagem: preListaPostagem;
    FpreListaPostagem_Specified: boolean;
    FstatusEtiqueta: statusObjetoPostal;
    FstatusEtiqueta_Specified: boolean;
    procedure SetcodigoEtiqueta(Index: Integer; const Astring: string);
    function  codigoEtiqueta_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacaoCliente(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacaoCliente_Specified(Index: Integer): boolean;
    procedure SetdataCancelamentoEtiqueta(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataCancelamentoEtiqueta_Specified(Index: Integer): boolean;
    procedure SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataInclusao_Specified(Index: Integer): boolean;
    procedure SetobjetoPostalPK(Index: Integer; const AobjetoPostalPK: objetoPostalPK);
    function  objetoPostalPK_Specified(Index: Integer): boolean;
    procedure SetplpNu(Index: Integer; const AInt64: Int64);
    function  plpNu_Specified(Index: Integer): boolean;
    procedure SetpreListaPostagem(Index: Integer; const ApreListaPostagem: preListaPostagem);
    function  preListaPostagem_Specified(Index: Integer): boolean;
    procedure SetstatusEtiqueta(Index: Integer; const AstatusObjetoPostal: statusObjetoPostal);
    function  statusEtiqueta_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property codigoEtiqueta:           string              Index (IS_OPTN or IS_UNQL) read FcodigoEtiqueta write SetcodigoEtiqueta stored codigoEtiqueta_Specified;
    property dataAtualizacaoCliente:   TXSDateTime         Index (IS_OPTN or IS_UNQL) read FdataAtualizacaoCliente write SetdataAtualizacaoCliente stored dataAtualizacaoCliente_Specified;
    property dataCancelamentoEtiqueta: TXSDateTime         Index (IS_OPTN or IS_UNQL) read FdataCancelamentoEtiqueta write SetdataCancelamentoEtiqueta stored dataCancelamentoEtiqueta_Specified;
    property dataInclusao:             TXSDateTime         Index (IS_OPTN or IS_UNQL) read FdataInclusao write SetdataInclusao stored dataInclusao_Specified;
    property objetoPostalPK:           objetoPostalPK      Index (IS_OPTN or IS_UNQL) read FobjetoPostalPK write SetobjetoPostalPK stored objetoPostalPK_Specified;
    property plpNu:                    Int64               Index (IS_OPTN or IS_UNQL) read FplpNu write SetplpNu stored plpNu_Specified;
    property preListaPostagem:         preListaPostagem    Index (IS_OPTN or IS_UNQL) read FpreListaPostagem write SetpreListaPostagem stored preListaPostagem_Specified;
    property statusEtiqueta:           statusObjetoPostal  Index (IS_OPTN or IS_UNQL) read FstatusEtiqueta write SetstatusEtiqueta stored statusEtiqueta_Specified;
  end;



  // ************************************************************************ //
  // XML       : usuarioInstalacao, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  usuarioInstalacao = class(TRemotable)
  private
    FdataAtualizacao: TXSDateTime;
    FdataAtualizacao_Specified: boolean;
    FdataInclusao: TXSDateTime;
    FdataInclusao_Specified: boolean;
    FdataSenha: TXSDateTime;
    FdataSenha_Specified: boolean;
    FgerenteMaster: gerenteConta;
    FgerenteMaster_Specified: boolean;
    FhashSenha: string;
    FhashSenha_Specified: boolean;
    Fid: Int64;
    Fid_Specified: boolean;
    Flogin: string;
    Flogin_Specified: boolean;
    Fnome: string;
    Fnome_Specified: boolean;
    Fparametros: Array_Of_parametroMaster;
    Fparametros_Specified: boolean;
    Fsenha: string;
    Fsenha_Specified: boolean;
    Fstatus: statusUsuario;
    Fstatus_Specified: boolean;
    Fvalidade: string;
    Fvalidade_Specified: boolean;
    procedure SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacao_Specified(Index: Integer): boolean;
    procedure SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataInclusao_Specified(Index: Integer): boolean;
    procedure SetdataSenha(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataSenha_Specified(Index: Integer): boolean;
    procedure SetgerenteMaster(Index: Integer; const AgerenteConta: gerenteConta);
    function  gerenteMaster_Specified(Index: Integer): boolean;
    procedure SethashSenha(Index: Integer; const Astring: string);
    function  hashSenha_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const AInt64: Int64);
    function  id_Specified(Index: Integer): boolean;
    procedure Setlogin(Index: Integer; const Astring: string);
    function  login_Specified(Index: Integer): boolean;
    procedure Setnome(Index: Integer; const Astring: string);
    function  nome_Specified(Index: Integer): boolean;
    procedure Setparametros(Index: Integer; const AArray_Of_parametroMaster: Array_Of_parametroMaster);
    function  parametros_Specified(Index: Integer): boolean;
    procedure Setsenha(Index: Integer; const Astring: string);
    function  senha_Specified(Index: Integer): boolean;
    procedure Setstatus(Index: Integer; const AstatusUsuario: statusUsuario);
    function  status_Specified(Index: Integer): boolean;
    procedure Setvalidade(Index: Integer; const Astring: string);
    function  validade_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property dataAtualizacao: TXSDateTime               Index (IS_OPTN or IS_UNQL) read FdataAtualizacao write SetdataAtualizacao stored dataAtualizacao_Specified;
    property dataInclusao:    TXSDateTime               Index (IS_OPTN or IS_UNQL) read FdataInclusao write SetdataInclusao stored dataInclusao_Specified;
    property dataSenha:       TXSDateTime               Index (IS_OPTN or IS_UNQL) read FdataSenha write SetdataSenha stored dataSenha_Specified;
    property gerenteMaster:   gerenteConta              Index (IS_OPTN or IS_UNQL) read FgerenteMaster write SetgerenteMaster stored gerenteMaster_Specified;
    property hashSenha:       string                    Index (IS_OPTN or IS_UNQL) read FhashSenha write SethashSenha stored hashSenha_Specified;
    property id:              Int64                     Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property login:           string                    Index (IS_OPTN or IS_UNQL) read Flogin write Setlogin stored login_Specified;
    property nome:            string                    Index (IS_OPTN or IS_UNQL) read Fnome write Setnome stored nome_Specified;
    property parametros:      Array_Of_parametroMaster  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fparametros write Setparametros stored parametros_Specified;
    property senha:           string                    Index (IS_OPTN or IS_UNQL) read Fsenha write Setsenha stored senha_Specified;
    property status:          statusUsuario             Index (IS_OPTN or IS_UNQL) read Fstatus write Setstatus stored status_Specified;
    property validade:        string                    Index (IS_OPTN or IS_UNQL) read Fvalidade write Setvalidade stored validade_Specified;
  end;



  // ************************************************************************ //
  // XML       : preListaPostagem, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  preListaPostagem = class(TRemotable)
  private
    FcartaoPostagem: cartaoPostagemERP;
    FcartaoPostagem_Specified: boolean;
    FdataAtualizacaoCliente: TXSDateTime;
    FdataAtualizacaoCliente_Specified: boolean;
    FdataAtualizacaoSara: TXSDateTime;
    FdataAtualizacaoSara_Specified: boolean;
    FdataFechamento: TXSDateTime;
    FdataFechamento_Specified: boolean;
    FdataPostagem: TXSDateTime;
    FdataPostagem_Specified: boolean;
    FdataPostagemSara: TXSDateTime;
    FdataPostagemSara_Specified: boolean;
    FobjetosPostais: Array_Of_objetoPostal;
    FobjetosPostais_Specified: boolean;
    FplpCliente: Int64;
    FplpNu: Int64;
    FplpXml: Array_Of_unsignedShort;
    FplpXml_Specified: boolean;
    FplpXmlRetorno: Array_Of_unsignedShort;
    FplpXmlRetorno_Specified: boolean;
    Fstatus: statusPlp;
    Fstatus_Specified: boolean;
    procedure SetcartaoPostagem(Index: Integer; const AcartaoPostagemERP: cartaoPostagemERP);
    function  cartaoPostagem_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacaoCliente(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacaoCliente_Specified(Index: Integer): boolean;
    procedure SetdataAtualizacaoSara(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataAtualizacaoSara_Specified(Index: Integer): boolean;
    procedure SetdataFechamento(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataFechamento_Specified(Index: Integer): boolean;
    procedure SetdataPostagem(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataPostagem_Specified(Index: Integer): boolean;
    procedure SetdataPostagemSara(Index: Integer; const ATXSDateTime: TXSDateTime);
    function  dataPostagemSara_Specified(Index: Integer): boolean;
    procedure SetobjetosPostais(Index: Integer; const AArray_Of_objetoPostal: Array_Of_objetoPostal);
    function  objetosPostais_Specified(Index: Integer): boolean;
    procedure SetplpXml(Index: Integer; const AArray_Of_unsignedShort: Array_Of_unsignedShort);
    function  plpXml_Specified(Index: Integer): boolean;
    procedure SetplpXmlRetorno(Index: Integer; const AArray_Of_unsignedShort: Array_Of_unsignedShort);
    function  plpXmlRetorno_Specified(Index: Integer): boolean;
    procedure Setstatus(Index: Integer; const AstatusPlp: statusPlp);
    function  status_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cartaoPostagem:         cartaoPostagemERP       Index (IS_OPTN or IS_UNQL) read FcartaoPostagem write SetcartaoPostagem stored cartaoPostagem_Specified;
    property dataAtualizacaoCliente: TXSDateTime             Index (IS_OPTN or IS_UNQL) read FdataAtualizacaoCliente write SetdataAtualizacaoCliente stored dataAtualizacaoCliente_Specified;
    property dataAtualizacaoSara:    TXSDateTime             Index (IS_OPTN or IS_UNQL) read FdataAtualizacaoSara write SetdataAtualizacaoSara stored dataAtualizacaoSara_Specified;
    property dataFechamento:         TXSDateTime             Index (IS_OPTN or IS_UNQL) read FdataFechamento write SetdataFechamento stored dataFechamento_Specified;
    property dataPostagem:           TXSDateTime             Index (IS_OPTN or IS_UNQL) read FdataPostagem write SetdataPostagem stored dataPostagem_Specified;
    property dataPostagemSara:       TXSDateTime             Index (IS_OPTN or IS_UNQL) read FdataPostagemSara write SetdataPostagemSara stored dataPostagemSara_Specified;
    property objetosPostais:         Array_Of_objetoPostal   Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FobjetosPostais write SetobjetosPostais stored objetosPostais_Specified;
    property plpCliente:             Int64                   Index (IS_UNQL) read FplpCliente write FplpCliente;
    property plpNu:                  Int64                   Index (IS_UNQL) read FplpNu write FplpNu;
    property plpXml:                 Array_Of_unsignedShort  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FplpXml write SetplpXml stored plpXml_Specified;
    property plpXmlRetorno:          Array_Of_unsignedShort  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read FplpXmlRetorno write SetplpXmlRetorno stored plpXmlRetorno_Specified;
    property status:                 statusPlp               Index (IS_OPTN or IS_UNQL) read Fstatus write Setstatus stored status_Specified;
  end;



  // ************************************************************************ //
  // XML       : objetoPostalPK, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  objetoPostalPK = class(TRemotable)
  private
    FcodigoEtiqueta: string;
    FcodigoEtiqueta_Specified: boolean;
    FplpNu: Int64;
    procedure SetcodigoEtiqueta(Index: Integer; const Astring: string);
    function  codigoEtiqueta_Specified(Index: Integer): boolean;
  published
    property codigoEtiqueta: string  Index (IS_OPTN or IS_UNQL) read FcodigoEtiqueta write SetcodigoEtiqueta stored codigoEtiqueta_Specified;
    property plpNu:          Int64   Index (IS_UNQL) read FplpNu write FplpNu;
  end;

  geraDigitoVerificadorEtiquetasResponse = array of Integer;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : objetoTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  objetoTO = class(TRemotable)
  private
    Fdesc: string;
    Fdesc_Specified: boolean;
    Fentrega: string;
    Fentrega_Specified: boolean;
    Fid: string;
    Fid_Specified: boolean;
    Fitem: string;
    Fitem_Specified: boolean;
    Fnum: string;
    Fnum_Specified: boolean;
    procedure Setdesc(Index: Integer; const Astring: string);
    function  desc_Specified(Index: Integer): boolean;
    procedure Setentrega(Index: Integer; const Astring: string);
    function  entrega_Specified(Index: Integer): boolean;
    procedure Setid(Index: Integer; const Astring: string);
    function  id_Specified(Index: Integer): boolean;
    procedure Setitem(Index: Integer; const Astring: string);
    function  item_Specified(Index: Integer): boolean;
    procedure Setnum(Index: Integer; const Astring: string);
    function  num_Specified(Index: Integer): boolean;
  published
    property desc:    string  Index (IS_OPTN or IS_UNQL) read Fdesc write Setdesc stored desc_Specified;
    property entrega: string  Index (IS_OPTN or IS_UNQL) read Fentrega write Setentrega stored entrega_Specified;
    property id:      string  Index (IS_OPTN or IS_UNQL) read Fid write Setid stored id_Specified;
    property item:    string  Index (IS_OPTN or IS_UNQL) read Fitem write Setitem stored item_Specified;
    property num:     string  Index (IS_OPTN or IS_UNQL) read Fnum write Setnum stored num_Specified;
  end;



  // ************************************************************************ //
  // XML       : coletaTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  coletaTO = class(TRemotable)
  private
    Fcklist: string;
    Fcklist_Specified: boolean;
    Fdescricao: string;
    Fdescricao_Specified: boolean;
    Fid_cliente: string;
    Fid_cliente_Specified: boolean;
    Fproduto: Array_Of_produtoTO;
    Fproduto_Specified: boolean;
    Fremetente: remetenteTO;
    Fremetente_Specified: boolean;
    Ftipo: string;
    Ftipo_Specified: boolean;
    Fvalor_declarado: string;
    Fvalor_declarado_Specified: boolean;
    procedure Setcklist(Index: Integer; const Astring: string);
    function  cklist_Specified(Index: Integer): boolean;
    procedure Setdescricao(Index: Integer; const Astring: string);
    function  descricao_Specified(Index: Integer): boolean;
    procedure Setid_cliente(Index: Integer; const Astring: string);
    function  id_cliente_Specified(Index: Integer): boolean;
    procedure Setproduto(Index: Integer; const AArray_Of_produtoTO: Array_Of_produtoTO);
    function  produto_Specified(Index: Integer): boolean;
    procedure Setremetente(Index: Integer; const AremetenteTO: remetenteTO);
    function  remetente_Specified(Index: Integer): boolean;
    procedure Settipo(Index: Integer; const Astring: string);
    function  tipo_Specified(Index: Integer): boolean;
    procedure Setvalor_declarado(Index: Integer; const Astring: string);
    function  valor_declarado_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property cklist:          string              Index (IS_OPTN or IS_UNQL) read Fcklist write Setcklist stored cklist_Specified;
    property descricao:       string              Index (IS_OPTN or IS_UNQL) read Fdescricao write Setdescricao stored descricao_Specified;
    property id_cliente:      string              Index (IS_OPTN or IS_UNQL) read Fid_cliente write Setid_cliente stored id_cliente_Specified;
    property produto:         Array_Of_produtoTO  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fproduto write Setproduto stored produto_Specified;
    property remetente:       remetenteTO         Index (IS_OPTN or IS_UNQL) read Fremetente write Setremetente stored remetente_Specified;
    property tipo:            string              Index (IS_OPTN or IS_UNQL) read Ftipo write Settipo stored tipo_Specified;
    property valor_declarado: string              Index (IS_OPTN or IS_UNQL) read Fvalor_declarado write Setvalor_declarado stored valor_declarado_Specified;
  end;



  // ************************************************************************ //
  // XML       : coletaReversaTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  coletaReversaTO = class(coletaTO)
  private
    Fag: string;
    Fag_Specified: boolean;
    Far: Integer;
    Far_Specified: boolean;
    Fcartao: Int64;
    Fcartao_Specified: boolean;
    Fnumero: Integer;
    Fnumero_Specified: boolean;
    Fobj_col: Array_Of_objetoTO;
    Fobj_col_Specified: boolean;
    Fservico_adicional: string;
    Fservico_adicional_Specified: boolean;
    procedure Setag(Index: Integer; const Astring: string);
    function  ag_Specified(Index: Integer): boolean;
    procedure Setar(Index: Integer; const AInteger: Integer);
    function  ar_Specified(Index: Integer): boolean;
    procedure Setcartao(Index: Integer; const AInt64: Int64);
    function  cartao_Specified(Index: Integer): boolean;
    procedure Setnumero(Index: Integer; const AInteger: Integer);
    function  numero_Specified(Index: Integer): boolean;
    procedure Setobj_col(Index: Integer; const AArray_Of_objetoTO: Array_Of_objetoTO);
    function  obj_col_Specified(Index: Integer): boolean;
    procedure Setservico_adicional(Index: Integer; const Astring: string);
    function  servico_adicional_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property ag:                string             Index (IS_OPTN or IS_UNQL) read Fag write Setag stored ag_Specified;
    property ar:                Integer            Index (IS_OPTN or IS_UNQL) read Far write Setar stored ar_Specified;
    property cartao:            Int64              Index (IS_OPTN or IS_UNQL) read Fcartao write Setcartao stored cartao_Specified;
    property numero:            Integer            Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
    property obj_col:           Array_Of_objetoTO  Index (IS_OPTN or IS_UNBD or IS_NLBL or IS_UNQL) read Fobj_col write Setobj_col stored obj_col_Specified;
    property servico_adicional: string             Index (IS_OPTN or IS_UNQL) read Fservico_adicional write Setservico_adicional stored servico_adicional_Specified;
  end;



  // ************************************************************************ //
  // XML       : coletaSimultaneaTO, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  coletaSimultaneaTO = class(coletaTO)
  private
    Fobj: string;
    Fobj_Specified: boolean;
    Fobs: string;
    Fobs_Specified: boolean;
    procedure Setobj(Index: Integer; const Astring: string);
    function  obj_Specified(Index: Integer): boolean;
    procedure Setobs(Index: Integer; const Astring: string);
    function  obs_Specified(Index: Integer): boolean;
  published
    property obj: string  Index (IS_OPTN or IS_UNQL) read Fobj write Setobj stored obj_Specified;
    property obs: string  Index (IS_OPTN or IS_UNQL) read Fobs write Setobs stored obs_Specified;
  end;

  obterMensagemRetornoPIResponse = array of mensagemRetornoPIMaster;   { "http://cliente.bean.master.sigep.bsb.correios.com.br/"[Lit][GblCplx] }


  // ************************************************************************ //
  // XML       : pedidoInformacaoConsulta, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  pedidoInformacaoConsulta = class(pedidoInformacao)
  private
    Fnumero: Int64;
    Fnumero_Specified: boolean;
    procedure Setnumero(Index: Integer; const AInt64: Int64);
    function  numero_Specified(Index: Integer): boolean;
  published
    property numero: Int64  Index (IS_OPTN or IS_UNQL) read Fnumero write Setnumero stored numero_Specified;
  end;



  // ************************************************************************ //
  // XML       : mensagemRetornoPIMaster, global, <complexType>
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // ************************************************************************ //
  mensagemRetornoPIMaster = class(TRemotable)
  private
    Fcodigo: Integer;
    Fcodigo_Specified: boolean;
    Fmensagem: string;
    Fmensagem_Specified: boolean;
    procedure Setcodigo(Index: Integer; const AInteger: Integer);
    function  codigo_Specified(Index: Integer): boolean;
    procedure Setmensagem(Index: Integer; const Astring: string);
    function  mensagem_Specified(Index: Integer): boolean;
  published
    property codigo:   Integer  Index (IS_OPTN or IS_UNQL) read Fcodigo write Setcodigo stored codigo_Specified;
    property mensagem: string   Index (IS_OPTN or IS_UNQL) read Fmensagem write Setmensagem stored mensagem_Specified;
  end;


  // ************************************************************************ //
  // Namespace : http://cliente.bean.master.sigep.bsb.correios.com.br/
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // use       : literal
  // binding   : AtendeClienteServiceSoapBinding
  // service   : AtendeClienteService
  // port      : AtendeClientePort
  // URL       : https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente
  // ************************************************************************ //
  AtendeCliente = interface(IInvokable)
  ['{06395354-FEDE-FDC9-368D-51C37A49D0E0}']
    function  fechaPlp(const xml: string; const idPlpCliente: Int64; const cartaoPostagem: string; const faixaEtiquetas: string; const usuario: string; const senha: string
                       ): Int64; stdcall;
    function  registrarPedidosInformacao(const pedidosInformacao: Array_Of_pedidoInformacaoRegistro; const usuario: string; const senha: string): registrarPedidosInformacaoResponse; stdcall;
    function  buscaCliente(const idContrato: string; const idCartaoPostagem: string; const usuario: string; const senha: string): clienteERP; stdcall;
    function  validaEtiquetaPLP(const numeroEtiqueta: string; const idPlp: Int64; const usuario: string; const senha: string): Boolean; stdcall;
    function  verificaDisponibilidadeServico(const codAdministrativo: Integer; const numeroServico: string; const cepOrigem: string; const cepDestino: string; const usuario: string; const senha: string
                                             ): Boolean; stdcall;
    function  getStatusPLP(const arg0: Array_Of_objetoPostal; const arg1: string): statusPlp; stdcall;
    function  bloquearObjeto(const numeroEtiqueta: string; const idPlp: Int64; const tipoBloqueio: tipoBloqueio; const acao: acao; const usuario: string; const senha: string
                             ): string; stdcall;
    function  solicitaEtiquetas(const tipoDestinatario: string; const identificador: string; const idServico: Int64; const qtdEtiquetas: Integer; const usuario: string; const senha: string
                                ): string; stdcall;
    function  obterMensagemRetornoPI: obterMensagemRetornoPIResponse; stdcall;
    function  consultarPedidosInformacao(const pedidosInformacao: Array_Of_pedidoInformacaoConsulta; const usuario: string; const senha: string): registrarPedidosInformacaoResponse; stdcall;
    function  buscaPagamentoEntrega(const usuario: string; const senha: string): string; stdcall;
    function  geraDigitoVerificadorEtiquetas(const etiquetas: Array_Of_string; const usuario: string; const senha: string): geraDigitoVerificadorEtiquetasResponse; stdcall;
    function  validarPostagemReversa(const codAdministrativo: Integer; const codigoServico: Integer; const cepDestinatario: string; const coleta: coletaReversaTO; const usuario: string; const senha: string
                                     ): Boolean; stdcall;
    function  fechaPlpVariosServicos(const xml: string; const idPlpCliente: Int64; const cartaoPostagem: string; const listaEtiquetas: Array_Of_string; const usuario: string; const senha: string
                                     ): Int64; stdcall;
    function  cancelarObjeto(const idPlp: Int64; const numeroEtiqueta: string; const usuario: string; const senha: string): Boolean; stdcall;
    function  validaPlp(const cliente: Int64; const numero: string; const diretoria: Int64; const cartao: string; const unidadePostagem: string; const servico: Int64; 
                        const servicosAdicionais: Array_Of_string; const usuario: string; const senha: string): Boolean; stdcall;
    function  validarPostagemSimultanea(const codAdministrativo: Integer; const codigoServico: Integer; const cepDestinatario: string; const coleta: coletaSimultaneaTO; const usuario: string; const senha: string
                                        ): Boolean; stdcall;
    function  obterEmbalagemLRS: obterEmbalagemLRSResponse; stdcall;
    function  cancelarPedidoScol(const codAdministrativo: Integer; const idPostagem: string; const tipo: string; const usuario: string; const senha: string): retornoCancelamentoTO; stdcall;
    function  buscaServicos(const idContrato: string; const idCartaoPostagem: string; const usuario: string; const senha: string): buscaServicosResponse; stdcall;
    function  solicitarPostagemScol(const codAdministrativo: Integer; const xml: string; const usuario: string; const senha: string): string; stdcall;
    function  solicitaPLP(const idPlpMaster: Int64; const numEtiqueta: string; const usuario: string; const senha: string): string; stdcall;
    function  getStatusCartaoPostagem(const numeroCartaoPostagem: string; const usuario: string; const senha: string): statusCartao; stdcall;
    function  solicitaXmlPlp(const idPlpMaster: Int64; const usuario: string; const senha: string): string; stdcall;
    function  obterMotivosPI: obterMotivosPIResponse; stdcall;
    function  buscaContrato(const numero: string; const diretoria: Int64; const usuario: string; const senha: string): contratoERP; stdcall;
    function  consultaSRO(const listaObjetos: Array_Of_string; const tipoConsulta: string; const tipoResultado: string; const usuarioSro: string; const senhaSro: string): string; stdcall;
    function  obterClienteAtualizacao(const cnpjCliente: string; const usuario: string; const senha: string): TXSDateTime; stdcall;
    function  integrarUsuarioScol(const codAdministrativo: Integer; const usuario: string; const senha: string): Boolean; stdcall;
    function  atualizaPLP(const idPlpMaster: Int64; const numEtiqueta: string; const usuario: string; const senha: string; const xml: string): Boolean; stdcall;
    function  obterAssuntosPI: obterAssuntosPIResponse; stdcall;
    function  consultaCEP(const cep: string): enderecoERP; stdcall;
  end;

function GetAtendeCliente(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): AtendeCliente;


implementation
  uses System.SysUtils;

function GetAtendeCliente(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): AtendeCliente;
const
  defWSDL = 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl';
  defURL  = 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente';
  defSvc  = 'AtendeClienteService';
  defPrt  = 'AtendeClientePort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as AtendeCliente);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


procedure cliente.SetnumeroContrato(Index: Integer; const Astring: string);
begin
  FnumeroContrato := Astring;
  FnumeroContrato_Specified := True;
end;

function cliente.numeroContrato_Specified(Index: Integer): boolean;
begin
  Result := FnumeroContrato_Specified;
end;

procedure cliente.SetpossuiContrato(Index: Integer; const Astring: string);
begin
  FpossuiContrato := Astring;
  FpossuiContrato_Specified := True;
end;

function cliente.possuiContrato_Specified(Index: Integer): boolean;
begin
  Result := FpossuiContrato_Specified;
end;

procedure conta.SetcodigoBanco(Index: Integer; const Astring: string);
begin
  FcodigoBanco := Astring;
  FcodigoBanco_Specified := True;
end;

function conta.codigoBanco_Specified(Index: Integer): boolean;
begin
  Result := FcodigoBanco_Specified;
end;

procedure conta.SetnomeBanco(Index: Integer; const Astring: string);
begin
  FnomeBanco := Astring;
  FnomeBanco_Specified := True;
end;

function conta.nomeBanco_Specified(Index: Integer): boolean;
begin
  Result := FnomeBanco_Specified;
end;

procedure conta.SetnumeroAgencia(Index: Integer; const Astring: string);
begin
  FnumeroAgencia := Astring;
  FnumeroAgencia_Specified := True;
end;

function conta.numeroAgencia_Specified(Index: Integer): boolean;
begin
  Result := FnumeroAgencia_Specified;
end;

procedure conta.SetnumeroConta(Index: Integer; const Astring: string);
begin
  FnumeroConta := Astring;
  FnumeroConta_Specified := True;
end;

function conta.numeroConta_Specified(Index: Integer): boolean;
begin
  Result := FnumeroConta_Specified;
end;

destructor enderecoERP.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FunidadesPostagem)-1 do
    System.SysUtils.FreeAndNil(FunidadesPostagem[I]);
  System.SetLength(FunidadesPostagem, 0);
  inherited Destroy;
end;

procedure enderecoERP.Setbairro(Index: Integer; const Astring: string);
begin
  Fbairro := Astring;
  Fbairro_Specified := True;
end;

function enderecoERP.bairro_Specified(Index: Integer): boolean;
begin
  Result := Fbairro_Specified;
end;

procedure enderecoERP.Setcep(Index: Integer; const Astring: string);
begin
  Fcep := Astring;
  Fcep_Specified := True;
end;

function enderecoERP.cep_Specified(Index: Integer): boolean;
begin
  Result := Fcep_Specified;
end;

procedure enderecoERP.Setcidade(Index: Integer; const Astring: string);
begin
  Fcidade := Astring;
  Fcidade_Specified := True;
end;

function enderecoERP.cidade_Specified(Index: Integer): boolean;
begin
  Result := Fcidade_Specified;
end;

procedure enderecoERP.Setcomplemento(Index: Integer; const Astring: string);
begin
  Fcomplemento := Astring;
  Fcomplemento_Specified := True;
end;

function enderecoERP.complemento_Specified(Index: Integer): boolean;
begin
  Result := Fcomplemento_Specified;
end;

procedure enderecoERP.Setcomplemento2(Index: Integer; const Astring: string);
begin
  Fcomplemento2 := Astring;
  Fcomplemento2_Specified := True;
end;

function enderecoERP.complemento2_Specified(Index: Integer): boolean;
begin
  Result := Fcomplemento2_Specified;
end;

procedure enderecoERP.Setend_(Index: Integer; const Astring: string);
begin
  Fend_ := Astring;
  Fend__Specified := True;
end;

function enderecoERP.end__Specified(Index: Integer): boolean;
begin
  Result := Fend__Specified;
end;

procedure enderecoERP.Setuf(Index: Integer; const Astring: string);
begin
  Fuf := Astring;
  Fuf_Specified := True;
end;

function enderecoERP.uf_Specified(Index: Integer): boolean;
begin
  Result := Fuf_Specified;
end;

procedure enderecoERP.SetunidadesPostagem(Index: Integer; const AArray_Of_unidadePostagemERP: Array_Of_unidadePostagemERP);
begin
  FunidadesPostagem := AArray_Of_unidadePostagemERP;
  FunidadesPostagem_Specified := True;
end;

function enderecoERP.unidadesPostagem_Specified(Index: Integer): boolean;
begin
  Result := FunidadesPostagem_Specified;
end;

procedure contratoERPPK.Setnumero(Index: Integer; const Astring: string);
begin
  Fnumero := Astring;
  Fnumero_Specified := True;
end;

function contratoERPPK.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

destructor unidadePostagemERP.Destroy;
begin
  System.SysUtils.FreeAndNil(Fendereco);
  inherited Destroy;
end;

procedure unidadePostagemERP.SetdiretoriaRegional(Index: Integer; const Astring: string);
begin
  FdiretoriaRegional := Astring;
  FdiretoriaRegional_Specified := True;
end;

function unidadePostagemERP.diretoriaRegional_Specified(Index: Integer): boolean;
begin
  Result := FdiretoriaRegional_Specified;
end;

procedure unidadePostagemERP.Setendereco(Index: Integer; const AenderecoERP: enderecoERP);
begin
  Fendereco := AenderecoERP;
  Fendereco_Specified := True;
end;

function unidadePostagemERP.endereco_Specified(Index: Integer): boolean;
begin
  Result := Fendereco_Specified;
end;

procedure unidadePostagemERP.Setid(Index: Integer; const Astring: string);
begin
  Fid := Astring;
  Fid_Specified := True;
end;

function unidadePostagemERP.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure unidadePostagemERP.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function unidadePostagemERP.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure unidadePostagemERP.Setstatus(Index: Integer; const Astring: string);
begin
  Fstatus := Astring;
  Fstatus_Specified := True;
end;

function unidadePostagemERP.status_Specified(Index: Integer): boolean;
begin
  Result := Fstatus_Specified;
end;

procedure unidadePostagemERP.Settipo(Index: Integer; const Astring: string);
begin
  Ftipo := Astring;
  Ftipo_Specified := True;
end;

function unidadePostagemERP.tipo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo_Specified;
end;

destructor servicoSigep.Destroy;
begin
  System.SysUtils.FreeAndNil(Fchancela);
  System.SysUtils.FreeAndNil(FservicoERP);
  inherited Destroy;
end;

procedure servicoSigep.SetcategoriaServico(Index: Integer; const AcategoriaServico: categoriaServico);
begin
  FcategoriaServico := AcategoriaServico;
  FcategoriaServico_Specified := True;
end;

function servicoSigep.categoriaServico_Specified(Index: Integer): boolean;
begin
  Result := FcategoriaServico_Specified;
end;

procedure servicoSigep.Setchancela(Index: Integer; const AchancelaMaster: chancelaMaster);
begin
  Fchancela := AchancelaMaster;
  Fchancela_Specified := True;
end;

function servicoSigep.chancela_Specified(Index: Integer): boolean;
begin
  Result := Fchancela_Specified;
end;

procedure servicoSigep.SetexigeDimensoes(Index: Integer; const ABoolean: Boolean);
begin
  FexigeDimensoes := ABoolean;
  FexigeDimensoes_Specified := True;
end;

function servicoSigep.exigeDimensoes_Specified(Index: Integer): boolean;
begin
  Result := FexigeDimensoes_Specified;
end;

procedure servicoSigep.SetexigeValorCobrar(Index: Integer; const ABoolean: Boolean);
begin
  FexigeValorCobrar := ABoolean;
  FexigeValorCobrar_Specified := True;
end;

function servicoSigep.exigeValorCobrar_Specified(Index: Integer): boolean;
begin
  Result := FexigeValorCobrar_Specified;
end;

procedure servicoSigep.SetservicoERP(Index: Integer; const AservicoERP: servicoERP);
begin
  FservicoERP := AservicoERP;
  FservicoERP_Specified := True;
end;

function servicoSigep.servicoERP_Specified(Index: Integer): boolean;
begin
  Result := FservicoERP_Specified;
end;

procedure servicoSigep.SetssiCoCodigoPostal(Index: Integer; const Astring: string);
begin
  FssiCoCodigoPostal := Astring;
  FssiCoCodigoPostal_Specified := True;
end;

function servicoSigep.ssiCoCodigoPostal_Specified(Index: Integer): boolean;
begin
  Result := FssiCoCodigoPostal_Specified;
end;

procedure SQLException.SeterrorCode(Index: Integer; const AInteger: Integer);
begin
  FerrorCode := AInteger;
  FerrorCode_Specified := True;
end;

function SQLException.errorCode_Specified(Index: Integer): boolean;
begin
  Result := FerrorCode_Specified;
end;

procedure SQLException.SetsQLState(Index: Integer; const Astring: string);
begin
  FsQLState := Astring;
  FsQLState_Specified := True;
end;

function SQLException.sQLState_Specified(Index: Integer): boolean;
begin
  Result := FsQLState_Specified;
end;

procedure SQLException.Setmessage_(Index: Integer; const Astring: string);
begin
  Fmessage_ := Astring;
  Fmessage__Specified := True;
end;

function SQLException.message__Specified(Index: Integer): boolean;
begin
  Result := Fmessage__Specified;
end;

procedure Exception.Setmessage_(Index: Integer; const Astring: string);
begin
  Fmessage_ := Astring;
  Fmessage__Specified := True;
end;

function Exception.message__Specified(Index: Integer): boolean;
begin
  Result := Fmessage__Specified;
end;

procedure pedidoInformacao.Setid(Index: Integer; const AInt64: Int64);
begin
  Fid := AInt64;
  Fid_Specified := True;
end;

function pedidoInformacao.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure pedidoInformacao.Setusuario(Index: Integer; const Astring: string);
begin
  Fusuario := Astring;
  Fusuario_Specified := True;
end;

function pedidoInformacao.usuario_Specified(Index: Integer): boolean;
begin
  Result := Fusuario_Specified;
end;

destructor pedidoInformacaoRegistro.Destroy;
begin
  System.SysUtils.FreeAndNil(Fcliente);
  System.SysUtils.FreeAndNil(Fconta);
  System.SysUtils.FreeAndNil(Fdestinatario);
  System.SysUtils.FreeAndNil(Fpostagem);
  System.SysUtils.FreeAndNil(Fremetente);
  inherited Destroy;
end;

procedure pedidoInformacaoRegistro.Setcliente(Index: Integer; const Acliente: cliente);
begin
  Fcliente := Acliente;
  Fcliente_Specified := True;
end;

function pedidoInformacaoRegistro.cliente_Specified(Index: Integer): boolean;
begin
  Result := Fcliente_Specified;
end;

procedure pedidoInformacaoRegistro.SetcodigoRegistro(Index: Integer; const Astring: string);
begin
  FcodigoRegistro := Astring;
  FcodigoRegistro_Specified := True;
end;

function pedidoInformacaoRegistro.codigoRegistro_Specified(Index: Integer): boolean;
begin
  Result := FcodigoRegistro_Specified;
end;

procedure pedidoInformacaoRegistro.Setconta(Index: Integer; const Aconta: conta);
begin
  Fconta := Aconta;
  Fconta_Specified := True;
end;

function pedidoInformacaoRegistro.conta_Specified(Index: Integer): boolean;
begin
  Result := Fconta_Specified;
end;

procedure pedidoInformacaoRegistro.SetconteudoObjeto(Index: Integer; const Astring: string);
begin
  FconteudoObjeto := Astring;
  FconteudoObjeto_Specified := True;
end;

function pedidoInformacaoRegistro.conteudoObjeto_Specified(Index: Integer): boolean;
begin
  Result := FconteudoObjeto_Specified;
end;

procedure pedidoInformacaoRegistro.SetcpfCnpj(Index: Integer; const Astring: string);
begin
  FcpfCnpj := Astring;
  FcpfCnpj_Specified := True;
end;

function pedidoInformacaoRegistro.cpfCnpj_Specified(Index: Integer): boolean;
begin
  Result := FcpfCnpj_Specified;
end;

procedure pedidoInformacaoRegistro.Setdestinatario(Index: Integer; const Adestinatario: destinatario);
begin
  Fdestinatario := Adestinatario;
  Fdestinatario_Specified := True;
end;

function pedidoInformacaoRegistro.destinatario_Specified(Index: Integer): boolean;
begin
  Result := Fdestinatario_Specified;
end;

procedure pedidoInformacaoRegistro.Setembalagem(Index: Integer; const Astring: string);
begin
  Fembalagem := Astring;
  Fembalagem_Specified := True;
end;

function pedidoInformacaoRegistro.embalagem_Specified(Index: Integer): boolean;
begin
  Result := Fembalagem_Specified;
end;

procedure pedidoInformacaoRegistro.Setmotivo(Index: Integer; const AInteger: Integer);
begin
  Fmotivo := AInteger;
  Fmotivo_Specified := True;
end;

function pedidoInformacaoRegistro.motivo_Specified(Index: Integer): boolean;
begin
  Result := Fmotivo_Specified;
end;

procedure pedidoInformacaoRegistro.Setobservacao(Index: Integer; const Astring: string);
begin
  Fobservacao := Astring;
  Fobservacao_Specified := True;
end;

function pedidoInformacaoRegistro.observacao_Specified(Index: Integer): boolean;
begin
  Result := Fobservacao_Specified;
end;

procedure pedidoInformacaoRegistro.Setpostagem(Index: Integer; const Apostagem: postagem);
begin
  Fpostagem := Apostagem;
  Fpostagem_Specified := True;
end;

function pedidoInformacaoRegistro.postagem_Specified(Index: Integer): boolean;
begin
  Result := Fpostagem_Specified;
end;

procedure pedidoInformacaoRegistro.Setremetente(Index: Integer; const Aremetente: remetente);
begin
  Fremetente := Aremetente;
  Fremetente_Specified := True;
end;

function pedidoInformacaoRegistro.remetente_Specified(Index: Integer): boolean;
begin
  Result := Fremetente_Specified;
end;

procedure pedidoInformacaoRegistro.Setservico(Index: Integer; const AInteger: Integer);
begin
  Fservico := AInteger;
  Fservico_Specified := True;
end;

function pedidoInformacaoRegistro.servico_Specified(Index: Integer): boolean;
begin
  Result := Fservico_Specified;
end;

procedure pedidoInformacaoRegistro.SettipoDocumento(Index: Integer; const Astring: string);
begin
  FtipoDocumento := Astring;
  FtipoDocumento_Specified := True;
end;

function pedidoInformacaoRegistro.tipoDocumento_Specified(Index: Integer): boolean;
begin
  Result := FtipoDocumento_Specified;
end;

procedure retorno.SetcodigoPI(Index: Integer; const AInt64: Int64);
begin
  FcodigoPI := AInt64;
  FcodigoPI_Specified := True;
end;

function retorno.codigoPI_Specified(Index: Integer): boolean;
begin
  Result := FcodigoPI_Specified;
end;

procedure retorno.SetcodigoRegistro(Index: Integer; const Astring: string);
begin
  FcodigoRegistro := Astring;
  FcodigoRegistro_Specified := True;
end;

function retorno.codigoRegistro_Specified(Index: Integer): boolean;
begin
  Result := FcodigoRegistro_Specified;
end;

procedure retorno.SetcodigoRetorno(Index: Integer; const Astring: string);
begin
  FcodigoRetorno := Astring;
  FcodigoRetorno_Specified := True;
end;

function retorno.codigoRetorno_Specified(Index: Integer): boolean;
begin
  Result := FcodigoRetorno_Specified;
end;

procedure retorno.SetdataPrazoResposta(Index: Integer; const Astring: string);
begin
  FdataPrazoResposta := Astring;
  FdataPrazoResposta_Specified := True;
end;

function retorno.dataPrazoResposta_Specified(Index: Integer): boolean;
begin
  Result := FdataPrazoResposta_Specified;
end;

procedure retorno.SetdataRegistro(Index: Integer; const Astring: string);
begin
  FdataRegistro := Astring;
  FdataRegistro_Specified := True;
end;

function retorno.dataRegistro_Specified(Index: Integer): boolean;
begin
  Result := FdataRegistro_Specified;
end;

procedure retorno.SetdataResposta(Index: Integer; const Astring: string);
begin
  FdataResposta := Astring;
  FdataResposta_Specified := True;
end;

function retorno.dataResposta_Specified(Index: Integer): boolean;
begin
  Result := FdataResposta_Specified;
end;

procedure retorno.SetdataUltimaRecorrencia(Index: Integer; const Astring: string);
begin
  FdataUltimaRecorrencia := Astring;
  FdataUltimaRecorrencia_Specified := True;
end;

function retorno.dataUltimaRecorrencia_Specified(Index: Integer): boolean;
begin
  Result := FdataUltimaRecorrencia_Specified;
end;

procedure retorno.Setid(Index: Integer; const AInt64: Int64);
begin
  Fid := AInt64;
  Fid_Specified := True;
end;

function retorno.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure retorno.SetmensagemRetorno(Index: Integer; const Astring: string);
begin
  FmensagemRetorno := Astring;
  FmensagemRetorno_Specified := True;
end;

function retorno.mensagemRetorno_Specified(Index: Integer): boolean;
begin
  Result := FmensagemRetorno_Specified;
end;

procedure retorno.Setresposta(Index: Integer; const Astring: string);
begin
  Fresposta := Astring;
  Fresposta_Specified := True;
end;

function retorno.resposta_Specified(Index: Integer): boolean;
begin
  Result := Fresposta_Specified;
end;

destructor clienteERP.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(Fcontratos)-1 do
    System.SysUtils.FreeAndNil(Fcontratos[I]);
  System.SetLength(Fcontratos, 0);
  for I := 0 to System.Length(FgerenteConta)-1 do
    System.SysUtils.FreeAndNil(FgerenteConta[I]);
  System.SetLength(FgerenteConta, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  inherited Destroy;
end;

procedure clienteERP.Setcnpj(Index: Integer; const Astring: string);
begin
  Fcnpj := Astring;
  Fcnpj_Specified := True;
end;

function clienteERP.cnpj_Specified(Index: Integer): boolean;
begin
  Result := Fcnpj_Specified;
end;

procedure clienteERP.Setcontratos(Index: Integer; const AArray_Of_contratoERP: Array_Of_contratoERP);
begin
  Fcontratos := AArray_Of_contratoERP;
  Fcontratos_Specified := True;
end;

function clienteERP.contratos_Specified(Index: Integer): boolean;
begin
  Result := Fcontratos_Specified;
end;

procedure clienteERP.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function clienteERP.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure clienteERP.SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FdatajAtualizacao := AInteger;
  FdatajAtualizacao_Specified := True;
end;

function clienteERP.datajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdatajAtualizacao_Specified;
end;

procedure clienteERP.SetdescricaoStatusCliente(Index: Integer; const Astring: string);
begin
  FdescricaoStatusCliente := Astring;
  FdescricaoStatusCliente_Specified := True;
end;

function clienteERP.descricaoStatusCliente_Specified(Index: Integer): boolean;
begin
  Result := FdescricaoStatusCliente_Specified;
end;

procedure clienteERP.SetgerenteConta(Index: Integer; const AArray_Of_gerenteConta: Array_Of_gerenteConta);
begin
  FgerenteConta := AArray_Of_gerenteConta;
  FgerenteConta_Specified := True;
end;

function clienteERP.gerenteConta_Specified(Index: Integer): boolean;
begin
  Result := FgerenteConta_Specified;
end;

procedure clienteERP.SethorajAtualizacao(Index: Integer; const AInt64: Int64);
begin
  FhorajAtualizacao := AInt64;
  FhorajAtualizacao_Specified := True;
end;

function clienteERP.horajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FhorajAtualizacao_Specified;
end;

procedure clienteERP.SetinscricaoEstadual(Index: Integer; const Astring: string);
begin
  FinscricaoEstadual := Astring;
  FinscricaoEstadual_Specified := True;
end;

function clienteERP.inscricaoEstadual_Specified(Index: Integer): boolean;
begin
  Result := FinscricaoEstadual_Specified;
end;

procedure clienteERP.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function clienteERP.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure clienteERP.SetstatusCodigo(Index: Integer; const Astring: string);
begin
  FstatusCodigo := Astring;
  FstatusCodigo_Specified := True;
end;

function clienteERP.statusCodigo_Specified(Index: Integer): boolean;
begin
  Result := FstatusCodigo_Specified;
end;

destructor chancelaMaster.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FservicosSigep)-1 do
    System.SysUtils.FreeAndNil(FservicosSigep[I]);
  System.SetLength(FservicosSigep, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  inherited Destroy;
end;

procedure chancelaMaster.Setchancela(Index: Integer; const ATByteDynArray: TByteDynArray);
begin
  Fchancela := ATByteDynArray;
  Fchancela_Specified := True;
end;

function chancelaMaster.chancela_Specified(Index: Integer): boolean;
begin
  Result := Fchancela_Specified;
end;

procedure chancelaMaster.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function chancelaMaster.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure chancelaMaster.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function chancelaMaster.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

procedure chancelaMaster.SetservicosSigep(Index: Integer; const AArray_Of_servicoSigep: Array_Of_servicoSigep);
begin
  FservicosSigep := AArray_Of_servicoSigep;
  FservicosSigep_Specified := True;
end;

function chancelaMaster.servicosSigep_Specified(Index: Integer): boolean;
begin
  Result := FservicosSigep_Specified;
end;

destructor gerenteConta.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FclientesVisiveis)-1 do
    System.SysUtils.FreeAndNil(FclientesVisiveis[I]);
  System.SetLength(FclientesVisiveis, 0);
  for I := 0 to System.Length(FusuariosInstalacao)-1 do
    System.SysUtils.FreeAndNil(FusuariosInstalacao[I]);
  System.SetLength(FusuariosInstalacao, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  System.SysUtils.FreeAndNil(FdataInclusao);
  System.SysUtils.FreeAndNil(FdataSenha);
  inherited Destroy;
end;

procedure gerenteConta.SetclientesVisiveis(Index: Integer; const AArray_Of_clienteERP: Array_Of_clienteERP);
begin
  FclientesVisiveis := AArray_Of_clienteERP;
  FclientesVisiveis_Specified := True;
end;

function gerenteConta.clientesVisiveis_Specified(Index: Integer): boolean;
begin
  Result := FclientesVisiveis_Specified;
end;

procedure gerenteConta.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function gerenteConta.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure gerenteConta.SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataInclusao := ATXSDateTime;
  FdataInclusao_Specified := True;
end;

function gerenteConta.dataInclusao_Specified(Index: Integer): boolean;
begin
  Result := FdataInclusao_Specified;
end;

procedure gerenteConta.SetdataSenha(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataSenha := ATXSDateTime;
  FdataSenha_Specified := True;
end;

function gerenteConta.dataSenha_Specified(Index: Integer): boolean;
begin
  Result := FdataSenha_Specified;
end;

procedure gerenteConta.Setlogin(Index: Integer; const Astring: string);
begin
  Flogin := Astring;
  Flogin_Specified := True;
end;

function gerenteConta.login_Specified(Index: Integer): boolean;
begin
  Result := Flogin_Specified;
end;

procedure gerenteConta.Setmatricula(Index: Integer; const Astring: string);
begin
  Fmatricula := Astring;
  Fmatricula_Specified := True;
end;

function gerenteConta.matricula_Specified(Index: Integer): boolean;
begin
  Result := Fmatricula_Specified;
end;

procedure gerenteConta.Setsenha(Index: Integer; const Astring: string);
begin
  Fsenha := Astring;
  Fsenha_Specified := True;
end;

function gerenteConta.senha_Specified(Index: Integer): boolean;
begin
  Result := Fsenha_Specified;
end;

procedure gerenteConta.Setstatus(Index: Integer; const AstatusGerente: statusGerente);
begin
  Fstatus := AstatusGerente;
  Fstatus_Specified := True;
end;

function gerenteConta.status_Specified(Index: Integer): boolean;
begin
  Result := Fstatus_Specified;
end;

procedure gerenteConta.SettipoGerente(Index: Integer; const AtipoGerente: tipoGerente);
begin
  FtipoGerente := AtipoGerente;
  FtipoGerente_Specified := True;
end;

function gerenteConta.tipoGerente_Specified(Index: Integer): boolean;
begin
  Result := FtipoGerente_Specified;
end;

procedure gerenteConta.SetusuariosInstalacao(Index: Integer; const AArray_Of_usuarioInstalacao: Array_Of_usuarioInstalacao);
begin
  FusuariosInstalacao := AArray_Of_usuarioInstalacao;
  FusuariosInstalacao_Specified := True;
end;

function gerenteConta.usuariosInstalacao_Specified(Index: Integer): boolean;
begin
  Result := FusuariosInstalacao_Specified;
end;

procedure gerenteConta.Setvalidade(Index: Integer; const Astring: string);
begin
  Fvalidade := Astring;
  Fvalidade_Specified := True;
end;

function gerenteConta.validade_Specified(Index: Integer): boolean;
begin
  Result := Fvalidade_Specified;
end;

destructor servicoAdicionalERP.Destroy;
begin
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  inherited Destroy;
end;

procedure servicoAdicionalERP.Setcodigo(Index: Integer; const Astring: string);
begin
  Fcodigo := Astring;
  Fcodigo_Specified := True;
end;

function servicoAdicionalERP.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure servicoAdicionalERP.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function servicoAdicionalERP.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure servicoAdicionalERP.SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FdatajAtualizacao := AInteger;
  FdatajAtualizacao_Specified := True;
end;

function servicoAdicionalERP.datajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdatajAtualizacao_Specified;
end;

procedure servicoAdicionalERP.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function servicoAdicionalERP.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

procedure servicoAdicionalERP.SethorajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FhorajAtualizacao := AInteger;
  FhorajAtualizacao_Specified := True;
end;

function servicoAdicionalERP.horajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FhorajAtualizacao_Specified;
end;

procedure servicoAdicionalERP.Setid(Index: Integer; const AInteger: Integer);
begin
  Fid := AInteger;
  Fid_Specified := True;
end;

function servicoAdicionalERP.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure servicoAdicionalERP.Setsigla(Index: Integer; const Astring: string);
begin
  Fsigla := Astring;
  Fsigla_Specified := True;
end;

function servicoAdicionalERP.sigla_Specified(Index: Integer): boolean;
begin
  Result := Fsigla_Specified;
end;

destructor vigenciaERP.Destroy;
begin
  System.SysUtils.FreeAndNil(FdataFinal);
  System.SysUtils.FreeAndNil(FdataInicial);
  inherited Destroy;
end;

procedure vigenciaERP.SetdataFinal(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataFinal := ATXSDateTime;
  FdataFinal_Specified := True;
end;

function vigenciaERP.dataFinal_Specified(Index: Integer): boolean;
begin
  Result := FdataFinal_Specified;
end;

procedure vigenciaERP.SetdataInicial(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataInicial := ATXSDateTime;
  FdataInicial_Specified := True;
end;

function vigenciaERP.dataInicial_Specified(Index: Integer): boolean;
begin
  Result := FdataInicial_Specified;
end;

procedure vigenciaERP.SetdatajFim(Index: Integer; const AInteger: Integer);
begin
  FdatajFim := AInteger;
  FdatajFim_Specified := True;
end;

function vigenciaERP.datajFim_Specified(Index: Integer): boolean;
begin
  Result := FdatajFim_Specified;
end;

procedure vigenciaERP.SetdatajIni(Index: Integer; const AInteger: Integer);
begin
  FdatajIni := AInteger;
  FdatajIni_Specified := True;
end;

function vigenciaERP.datajIni_Specified(Index: Integer): boolean;
begin
  Result := FdatajIni_Specified;
end;

procedure vigenciaERP.Setid(Index: Integer; const AInt64: Int64);
begin
  Fid := AInt64;
  Fid_Specified := True;
end;

function vigenciaERP.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

destructor servicoERP.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FservicosAdicionais)-1 do
    System.SysUtils.FreeAndNil(FservicosAdicionais[I]);
  System.SetLength(FservicosAdicionais, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  System.SysUtils.FreeAndNil(FservicoSigep);
  System.SysUtils.FreeAndNil(Fvigencia);
  inherited Destroy;
end;

procedure servicoERP.Setcodigo(Index: Integer; const Astring: string);
begin
  Fcodigo := Astring;
  Fcodigo_Specified := True;
end;

function servicoERP.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure servicoERP.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function servicoERP.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure servicoERP.SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FdatajAtualizacao := AInteger;
  FdatajAtualizacao_Specified := True;
end;

function servicoERP.datajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdatajAtualizacao_Specified;
end;

procedure servicoERP.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function servicoERP.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

procedure servicoERP.SethorajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FhorajAtualizacao := AInteger;
  FhorajAtualizacao_Specified := True;
end;

function servicoERP.horajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FhorajAtualizacao_Specified;
end;

procedure servicoERP.SetservicoSigep(Index: Integer; const AservicoSigep: servicoSigep);
begin
  FservicoSigep := AservicoSigep;
  FservicoSigep_Specified := True;
end;

function servicoERP.servicoSigep_Specified(Index: Integer): boolean;
begin
  Result := FservicoSigep_Specified;
end;

procedure servicoERP.SetservicosAdicionais(Index: Integer; const AArray_Of_servicoAdicionalERP: Array_Of_servicoAdicionalERP);
begin
  FservicosAdicionais := AArray_Of_servicoAdicionalERP;
  FservicosAdicionais_Specified := True;
end;

function servicoERP.servicosAdicionais_Specified(Index: Integer): boolean;
begin
  Result := FservicosAdicionais_Specified;
end;

procedure servicoERP.Settipo1Codigo(Index: Integer; const Astring: string);
begin
  Ftipo1Codigo := Astring;
  Ftipo1Codigo_Specified := True;
end;

function servicoERP.tipo1Codigo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo1Codigo_Specified;
end;

procedure servicoERP.Settipo1Descricao(Index: Integer; const Astring: string);
begin
  Ftipo1Descricao := Astring;
  Ftipo1Descricao_Specified := True;
end;

function servicoERP.tipo1Descricao_Specified(Index: Integer): boolean;
begin
  Result := Ftipo1Descricao_Specified;
end;

procedure servicoERP.Settipo2Codigo(Index: Integer; const Astring: string);
begin
  Ftipo2Codigo := Astring;
  Ftipo2Codigo_Specified := True;
end;

function servicoERP.tipo2Codigo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo2Codigo_Specified;
end;

procedure servicoERP.Settipo2Descricao(Index: Integer; const Astring: string);
begin
  Ftipo2Descricao := Astring;
  Ftipo2Descricao_Specified := True;
end;

function servicoERP.tipo2Descricao_Specified(Index: Integer): boolean;
begin
  Result := Ftipo2Descricao_Specified;
end;

procedure servicoERP.Setvigencia(Index: Integer; const AvigenciaERP: vigenciaERP);
begin
  Fvigencia := AvigenciaERP;
  Fvigencia_Specified := True;
end;

function servicoERP.vigencia_Specified(Index: Integer): boolean;
begin
  Result := Fvigencia_Specified;
end;

destructor contratoERP.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FcartoesPostagem)-1 do
    System.SysUtils.FreeAndNil(FcartoesPostagem[I]);
  System.SetLength(FcartoesPostagem, 0);
  System.SysUtils.FreeAndNil(Fcliente);
  System.SysUtils.FreeAndNil(FcontratoPK);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  System.SysUtils.FreeAndNil(FdataVigenciaFim);
  System.SysUtils.FreeAndNil(FdataVigenciaInicio);
  System.SysUtils.FreeAndNil(FdiretoriaRegional);
  inherited Destroy;
end;

procedure contratoERP.SetcartoesPostagem(Index: Integer; const AArray_Of_cartaoPostagemERP: Array_Of_cartaoPostagemERP);
begin
  FcartoesPostagem := AArray_Of_cartaoPostagemERP;
  FcartoesPostagem_Specified := True;
end;

function contratoERP.cartoesPostagem_Specified(Index: Integer): boolean;
begin
  Result := FcartoesPostagem_Specified;
end;

procedure contratoERP.Setcliente(Index: Integer; const AclienteERP: clienteERP);
begin
  Fcliente := AclienteERP;
  Fcliente_Specified := True;
end;

function contratoERP.cliente_Specified(Index: Integer): boolean;
begin
  Result := Fcliente_Specified;
end;

procedure contratoERP.SetcodigoDiretoria(Index: Integer; const Astring: string);
begin
  FcodigoDiretoria := Astring;
  FcodigoDiretoria_Specified := True;
end;

function contratoERP.codigoDiretoria_Specified(Index: Integer): boolean;
begin
  Result := FcodigoDiretoria_Specified;
end;

procedure contratoERP.SetcontratoPK(Index: Integer; const AcontratoERPPK: contratoERPPK);
begin
  FcontratoPK := AcontratoERPPK;
  FcontratoPK_Specified := True;
end;

function contratoERP.contratoPK_Specified(Index: Integer): boolean;
begin
  Result := FcontratoPK_Specified;
end;

procedure contratoERP.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function contratoERP.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure contratoERP.SetdataAtualizacaoDDMMYYYY(Index: Integer; const Astring: string);
begin
  FdataAtualizacaoDDMMYYYY := Astring;
  FdataAtualizacaoDDMMYYYY_Specified := True;
end;

function contratoERP.dataAtualizacaoDDMMYYYY_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacaoDDMMYYYY_Specified;
end;

procedure contratoERP.SetdataVigenciaFim(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataVigenciaFim := ATXSDateTime;
  FdataVigenciaFim_Specified := True;
end;

function contratoERP.dataVigenciaFim_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaFim_Specified;
end;

procedure contratoERP.SetdataVigenciaFimDDMMYYYY(Index: Integer; const Astring: string);
begin
  FdataVigenciaFimDDMMYYYY := Astring;
  FdataVigenciaFimDDMMYYYY_Specified := True;
end;

function contratoERP.dataVigenciaFimDDMMYYYY_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaFimDDMMYYYY_Specified;
end;

procedure contratoERP.SetdataVigenciaInicio(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataVigenciaInicio := ATXSDateTime;
  FdataVigenciaInicio_Specified := True;
end;

function contratoERP.dataVigenciaInicio_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaInicio_Specified;
end;

procedure contratoERP.SetdataVigenciaInicioDDMMYYYY(Index: Integer; const Astring: string);
begin
  FdataVigenciaInicioDDMMYYYY := Astring;
  FdataVigenciaInicioDDMMYYYY_Specified := True;
end;

function contratoERP.dataVigenciaInicioDDMMYYYY_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaInicioDDMMYYYY_Specified;
end;

procedure contratoERP.SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FdatajAtualizacao := AInteger;
  FdatajAtualizacao_Specified := True;
end;

function contratoERP.datajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdatajAtualizacao_Specified;
end;

procedure contratoERP.SetdatajVigenciaFim(Index: Integer; const AInteger: Integer);
begin
  FdatajVigenciaFim := AInteger;
  FdatajVigenciaFim_Specified := True;
end;

function contratoERP.datajVigenciaFim_Specified(Index: Integer): boolean;
begin
  Result := FdatajVigenciaFim_Specified;
end;

procedure contratoERP.SetdatajVigenciaInicio(Index: Integer; const AInteger: Integer);
begin
  FdatajVigenciaInicio := AInteger;
  FdatajVigenciaInicio_Specified := True;
end;

function contratoERP.datajVigenciaInicio_Specified(Index: Integer): boolean;
begin
  Result := FdatajVigenciaInicio_Specified;
end;

procedure contratoERP.SetdescricaoDiretoriaRegional(Index: Integer; const Astring: string);
begin
  FdescricaoDiretoriaRegional := Astring;
  FdescricaoDiretoriaRegional_Specified := True;
end;

function contratoERP.descricaoDiretoriaRegional_Specified(Index: Integer): boolean;
begin
  Result := FdescricaoDiretoriaRegional_Specified;
end;

procedure contratoERP.SetdescricaoStatus(Index: Integer; const Astring: string);
begin
  FdescricaoStatus := Astring;
  FdescricaoStatus_Specified := True;
end;

function contratoERP.descricaoStatus_Specified(Index: Integer): boolean;
begin
  Result := FdescricaoStatus_Specified;
end;

procedure contratoERP.SetdiretoriaRegional(Index: Integer; const AunidadePostagemERP: unidadePostagemERP);
begin
  FdiretoriaRegional := AunidadePostagemERP;
  FdiretoriaRegional_Specified := True;
end;

function contratoERP.diretoriaRegional_Specified(Index: Integer): boolean;
begin
  Result := FdiretoriaRegional_Specified;
end;

procedure contratoERP.SethorajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FhorajAtualizacao := AInteger;
  FhorajAtualizacao_Specified := True;
end;

function contratoERP.horajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FhorajAtualizacao_Specified;
end;

procedure contratoERP.SetstatusCodigo(Index: Integer; const Astring: string);
begin
  FstatusCodigo := Astring;
  FstatusCodigo_Specified := True;
end;

function contratoERP.statusCodigo_Specified(Index: Integer): boolean;
begin
  Result := FstatusCodigo_Specified;
end;

procedure postagem.Setagencia(Index: Integer; const Astring: string);
begin
  Fagencia := Astring;
  Fagencia_Specified := True;
end;

function postagem.agencia_Specified(Index: Integer): boolean;
begin
  Result := Fagencia_Specified;
end;

procedure postagem.SetavisoRecebimento(Index: Integer; const Astring: string);
begin
  FavisoRecebimento := Astring;
  FavisoRecebimento_Specified := True;
end;

function postagem.avisoRecebimento_Specified(Index: Integer): boolean;
begin
  Result := FavisoRecebimento_Specified;
end;

procedure postagem.Setdata(Index: Integer; const Astring: string);
begin
  Fdata := Astring;
  Fdata_Specified := True;
end;

function postagem.data_Specified(Index: Integer): boolean;
begin
  Result := Fdata_Specified;
end;

procedure postagem.Setlocal_(Index: Integer; const Astring: string);
begin
  Flocal_ := Astring;
  Flocal__Specified := True;
end;

function postagem.local__Specified(Index: Integer): boolean;
begin
  Result := Flocal__Specified;
end;

procedure postagem.SetvalorDeclarado(Index: Integer; const Astring: string);
begin
  FvalorDeclarado := Astring;
  FvalorDeclarado_Specified := True;
end;

function postagem.valorDeclarado_Specified(Index: Integer): boolean;
begin
  Result := FvalorDeclarado_Specified;
end;

procedure pessoa.Setbairro(Index: Integer; const Astring: string);
begin
  Fbairro := Astring;
  Fbairro_Specified := True;
end;

function pessoa.bairro_Specified(Index: Integer): boolean;
begin
  Result := Fbairro_Specified;
end;

procedure pessoa.Setcep(Index: Integer; const Astring: string);
begin
  Fcep := Astring;
  Fcep_Specified := True;
end;

function pessoa.cep_Specified(Index: Integer): boolean;
begin
  Result := Fcep_Specified;
end;

procedure pessoa.Setcidade(Index: Integer; const Astring: string);
begin
  Fcidade := Astring;
  Fcidade_Specified := True;
end;

function pessoa.cidade_Specified(Index: Integer): boolean;
begin
  Result := Fcidade_Specified;
end;

procedure pessoa.Setcomplemento(Index: Integer; const Astring: string);
begin
  Fcomplemento := Astring;
  Fcomplemento_Specified := True;
end;

function pessoa.complemento_Specified(Index: Integer): boolean;
begin
  Result := Fcomplemento_Specified;
end;

procedure pessoa.Setddd(Index: Integer; const Astring: string);
begin
  Fddd := Astring;
  Fddd_Specified := True;
end;

function pessoa.ddd_Specified(Index: Integer): boolean;
begin
  Result := Fddd_Specified;
end;

procedure pessoa.Setendereco(Index: Integer; const Astring: string);
begin
  Fendereco := Astring;
  Fendereco_Specified := True;
end;

function pessoa.endereco_Specified(Index: Integer): boolean;
begin
  Result := Fendereco_Specified;
end;

procedure pessoa.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function pessoa.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure pessoa.Setnumero(Index: Integer; const Astring: string);
begin
  Fnumero := Astring;
  Fnumero_Specified := True;
end;

function pessoa.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

procedure pessoa.Setpais(Index: Integer; const Astring: string);
begin
  Fpais := Astring;
  Fpais_Specified := True;
end;

function pessoa.pais_Specified(Index: Integer): boolean;
begin
  Result := Fpais_Specified;
end;

procedure pessoa.Settelefone(Index: Integer; const Astring: string);
begin
  Ftelefone := Astring;
  Ftelefone_Specified := True;
end;

function pessoa.telefone_Specified(Index: Integer): boolean;
begin
  Result := Ftelefone_Specified;
end;

procedure pessoa.Setuf(Index: Integer; const Astring: string);
begin
  Fuf := Astring;
  Fuf_Specified := True;
end;

function pessoa.uf_Specified(Index: Integer): boolean;
begin
  Result := Fuf_Specified;
end;

procedure remetente.Setemail(Index: Integer; const Astring: string);
begin
  Femail := Astring;
  Femail_Specified := True;
end;

function remetente.email_Specified(Index: Integer): boolean;
begin
  Result := Femail_Specified;
end;

procedure remetente.Setempresa(Index: Integer; const Astring: string);
begin
  Fempresa := Astring;
  Fempresa_Specified := True;
end;

function remetente.empresa_Specified(Index: Integer): boolean;
begin
  Result := Fempresa_Specified;
end;

procedure remetente.Setfax(Index: Integer; const Astring: string);
begin
  Ffax := Astring;
  Ffax_Specified := True;
end;

function remetente.fax_Specified(Index: Integer): boolean;
begin
  Result := Ffax_Specified;
end;

procedure parametroMaster.SetprmTxParametro(Index: Integer; const Astring: string);
begin
  FprmTxParametro := Astring;
  FprmTxParametro_Specified := True;
end;

function parametroMaster.prmTxParametro_Specified(Index: Integer): boolean;
begin
  Result := FprmTxParametro_Specified;
end;

procedure parametroMaster.SetprmTxValor(Index: Integer; const Astring: string);
begin
  FprmTxValor := Astring;
  FprmTxValor_Specified := True;
end;

function parametroMaster.prmTxValor_Specified(Index: Integer): boolean;
begin
  Result := FprmTxValor_Specified;
end;

procedure embalagemLRSMaster.Setcodigo(Index: Integer; const Astring: string);
begin
  Fcodigo := Astring;
  Fcodigo_Specified := True;
end;

function embalagemLRSMaster.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure embalagemLRSMaster.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function embalagemLRSMaster.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure embalagemLRSMaster.Settipo(Index: Integer; const Astring: string);
begin
  Ftipo := Astring;
  Ftipo_Specified := True;
end;

function embalagemLRSMaster.tipo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo_Specified;
end;

procedure objetoSimplificadoTO.Setdatahora_cancelamento(Index: Integer; const Astring: string);
begin
  Fdatahora_cancelamento := Astring;
  Fdatahora_cancelamento_Specified := True;
end;

function objetoSimplificadoTO.datahora_cancelamento_Specified(Index: Integer): boolean;
begin
  Result := Fdatahora_cancelamento_Specified;
end;

procedure objetoSimplificadoTO.Setnumero_pedido(Index: Integer; const AInteger: Integer);
begin
  Fnumero_pedido := AInteger;
  Fnumero_pedido_Specified := True;
end;

function objetoSimplificadoTO.numero_pedido_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_pedido_Specified;
end;

procedure objetoSimplificadoTO.Setstatus_pedido(Index: Integer; const Astring: string);
begin
  Fstatus_pedido := Astring;
  Fstatus_pedido_Specified := True;
end;

function objetoSimplificadoTO.status_pedido_Specified(Index: Integer): boolean;
begin
  Result := Fstatus_pedido_Specified;
end;

destructor retornoCancelamentoTO.Destroy;
begin
  System.SysUtils.FreeAndNil(Fobjeto_postal);
  inherited Destroy;
end;

procedure retornoCancelamentoTO.Setcod_erro(Index: Integer; const Astring: string);
begin
  Fcod_erro := Astring;
  Fcod_erro_Specified := True;
end;

function retornoCancelamentoTO.cod_erro_Specified(Index: Integer): boolean;
begin
  Result := Fcod_erro_Specified;
end;

procedure retornoCancelamentoTO.Setcodigo_administrativo(Index: Integer; const Astring: string);
begin
  Fcodigo_administrativo := Astring;
  Fcodigo_administrativo_Specified := True;
end;

function retornoCancelamentoTO.codigo_administrativo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_administrativo_Specified;
end;

procedure retornoCancelamentoTO.Setdata(Index: Integer; const Astring: string);
begin
  Fdata := Astring;
  Fdata_Specified := True;
end;

function retornoCancelamentoTO.data_Specified(Index: Integer): boolean;
begin
  Result := Fdata_Specified;
end;

procedure retornoCancelamentoTO.Sethora(Index: Integer; const Astring: string);
begin
  Fhora := Astring;
  Fhora_Specified := True;
end;

function retornoCancelamentoTO.hora_Specified(Index: Integer): boolean;
begin
  Result := Fhora_Specified;
end;

procedure retornoCancelamentoTO.Setmsg_erro(Index: Integer; const Astring: string);
begin
  Fmsg_erro := Astring;
  Fmsg_erro_Specified := True;
end;

function retornoCancelamentoTO.msg_erro_Specified(Index: Integer): boolean;
begin
  Result := Fmsg_erro_Specified;
end;

procedure retornoCancelamentoTO.Setobjeto_postal(Index: Integer; const AobjetoSimplificadoTO: objetoSimplificadoTO);
begin
  Fobjeto_postal := AobjetoSimplificadoTO;
  Fobjeto_postal_Specified := True;
end;

function retornoCancelamentoTO.objeto_postal_Specified(Index: Integer): boolean;
begin
  Result := Fobjeto_postal_Specified;
end;

procedure produtoTO.Setcodigo(Index: Integer; const AInt64: Int64);
begin
  Fcodigo := AInt64;
  Fcodigo_Specified := True;
end;

function produtoTO.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure produtoTO.Setqtd(Index: Integer; const AInt64: Int64);
begin
  Fqtd := AInt64;
  Fqtd_Specified := True;
end;

function produtoTO.qtd_Specified(Index: Integer): boolean;
begin
  Result := Fqtd_Specified;
end;

procedure produtoTO.Settipo(Index: Integer; const AInt64: Int64);
begin
  Ftipo := AInt64;
  Ftipo_Specified := True;
end;

function produtoTO.tipo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo_Specified;
end;

procedure pessoaTO.Setbairro(Index: Integer; const Astring: string);
begin
  Fbairro := Astring;
  Fbairro_Specified := True;
end;

function pessoaTO.bairro_Specified(Index: Integer): boolean;
begin
  Result := Fbairro_Specified;
end;

procedure pessoaTO.Setcep(Index: Integer; const Astring: string);
begin
  Fcep := Astring;
  Fcep_Specified := True;
end;

function pessoaTO.cep_Specified(Index: Integer): boolean;
begin
  Result := Fcep_Specified;
end;

procedure pessoaTO.Setcidade(Index: Integer; const Astring: string);
begin
  Fcidade := Astring;
  Fcidade_Specified := True;
end;

function pessoaTO.cidade_Specified(Index: Integer): boolean;
begin
  Result := Fcidade_Specified;
end;

procedure pessoaTO.Setcomplemento(Index: Integer; const Astring: string);
begin
  Fcomplemento := Astring;
  Fcomplemento_Specified := True;
end;

function pessoaTO.complemento_Specified(Index: Integer): boolean;
begin
  Result := Fcomplemento_Specified;
end;

procedure pessoaTO.Setddd(Index: Integer; const Astring: string);
begin
  Fddd := Astring;
  Fddd_Specified := True;
end;

function pessoaTO.ddd_Specified(Index: Integer): boolean;
begin
  Result := Fddd_Specified;
end;

procedure pessoaTO.Setemail(Index: Integer; const Astring: string);
begin
  Femail := Astring;
  Femail_Specified := True;
end;

function pessoaTO.email_Specified(Index: Integer): boolean;
begin
  Result := Femail_Specified;
end;

procedure pessoaTO.Setlogradouro(Index: Integer; const Astring: string);
begin
  Flogradouro := Astring;
  Flogradouro_Specified := True;
end;

function pessoaTO.logradouro_Specified(Index: Integer): boolean;
begin
  Result := Flogradouro_Specified;
end;

procedure pessoaTO.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function pessoaTO.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure pessoaTO.Setnumero(Index: Integer; const Astring: string);
begin
  Fnumero := Astring;
  Fnumero_Specified := True;
end;

function pessoaTO.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

procedure pessoaTO.Setreferencia(Index: Integer; const Astring: string);
begin
  Freferencia := Astring;
  Freferencia_Specified := True;
end;

function pessoaTO.referencia_Specified(Index: Integer): boolean;
begin
  Result := Freferencia_Specified;
end;

procedure pessoaTO.Settelefone(Index: Integer; const Astring: string);
begin
  Ftelefone := Astring;
  Ftelefone_Specified := True;
end;

function pessoaTO.telefone_Specified(Index: Integer): boolean;
begin
  Result := Ftelefone_Specified;
end;

procedure pessoaTO.Setuf(Index: Integer; const Astring: string);
begin
  Fuf := Astring;
  Fuf_Specified := True;
end;

function pessoaTO.uf_Specified(Index: Integer): boolean;
begin
  Result := Fuf_Specified;
end;

procedure remetenteTO.Setidentificacao(Index: Integer; const Astring: string);
begin
  Fidentificacao := Astring;
  Fidentificacao_Specified := True;
end;

function remetenteTO.identificacao_Specified(Index: Integer): boolean;
begin
  Result := Fidentificacao_Specified;
end;

procedure assuntoPIMaster.Setcodigo(Index: Integer; const AInteger: Integer);
begin
  Fcodigo := AInteger;
  Fcodigo_Specified := True;
end;

function assuntoPIMaster.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure assuntoPIMaster.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function assuntoPIMaster.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

procedure assuntoPIMaster.Settipo(Index: Integer; const Astring: string);
begin
  Ftipo := Astring;
  Ftipo_Specified := True;
end;

function assuntoPIMaster.tipo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo_Specified;
end;

procedure SQLException2.SeterrorCode(Index: Integer; const AInteger: Integer);
begin
  FerrorCode := AInteger;
  FerrorCode_Specified := True;
end;

function SQLException2.errorCode_Specified(Index: Integer): boolean;
begin
  Result := FerrorCode_Specified;
end;

procedure SQLException2.SetsQLState(Index: Integer; const Astring: string);
begin
  FsQLState := Astring;
  FsQLState_Specified := True;
end;

function SQLException2.sQLState_Specified(Index: Integer): boolean;
begin
  Result := FsQLState_Specified;
end;

procedure SQLException2.Setmessage_(Index: Integer; const Astring: string);
begin
  Fmessage_ := Astring;
  Fmessage__Specified := True;
end;

function SQLException2.message__Specified(Index: Integer): boolean;
begin
  Result := Fmessage__Specified;
end;

procedure Exception2.Setmessage_(Index: Integer; const Astring: string);
begin
  Fmessage_ := Astring;
  Fmessage__Specified := True;
end;

function Exception2.message__Specified(Index: Integer): boolean;
begin
  Result := Fmessage__Specified;
end;

destructor cartaoPostagemERP.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(Fcontratos)-1 do
    System.SysUtils.FreeAndNil(Fcontratos[I]);
  System.SetLength(Fcontratos, 0);
  for I := 0 to System.Length(Fservicos)-1 do
    System.SysUtils.FreeAndNil(Fservicos[I]);
  System.SetLength(Fservicos, 0);
  for I := 0 to System.Length(FunidadesPostagem)-1 do
    System.SysUtils.FreeAndNil(FunidadesPostagem[I]);
  System.SetLength(FunidadesPostagem, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  System.SysUtils.FreeAndNil(FdataVigenciaFim);
  System.SysUtils.FreeAndNil(FdataVigenciaInicio);
  inherited Destroy;
end;

procedure cartaoPostagemERP.SetcodigoAdministrativo(Index: Integer; const Astring: string);
begin
  FcodigoAdministrativo := Astring;
  FcodigoAdministrativo_Specified := True;
end;

function cartaoPostagemERP.codigoAdministrativo_Specified(Index: Integer): boolean;
begin
  Result := FcodigoAdministrativo_Specified;
end;

procedure cartaoPostagemERP.Setcontratos(Index: Integer; const AArray_Of_contratoERP: Array_Of_contratoERP);
begin
  Fcontratos := AArray_Of_contratoERP;
  Fcontratos_Specified := True;
end;

function cartaoPostagemERP.contratos_Specified(Index: Integer): boolean;
begin
  Result := Fcontratos_Specified;
end;

procedure cartaoPostagemERP.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function cartaoPostagemERP.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure cartaoPostagemERP.SetdataVigenciaFim(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataVigenciaFim := ATXSDateTime;
  FdataVigenciaFim_Specified := True;
end;

function cartaoPostagemERP.dataVigenciaFim_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaFim_Specified;
end;

procedure cartaoPostagemERP.SetdataVigenciaInicio(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataVigenciaInicio := ATXSDateTime;
  FdataVigenciaInicio_Specified := True;
end;

function cartaoPostagemERP.dataVigenciaInicio_Specified(Index: Integer): boolean;
begin
  Result := FdataVigenciaInicio_Specified;
end;

procedure cartaoPostagemERP.SetdatajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FdatajAtualizacao := AInteger;
  FdatajAtualizacao_Specified := True;
end;

function cartaoPostagemERP.datajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdatajAtualizacao_Specified;
end;

procedure cartaoPostagemERP.SetdatajVigenciaFim(Index: Integer; const AInteger: Integer);
begin
  FdatajVigenciaFim := AInteger;
  FdatajVigenciaFim_Specified := True;
end;

function cartaoPostagemERP.datajVigenciaFim_Specified(Index: Integer): boolean;
begin
  Result := FdatajVigenciaFim_Specified;
end;

procedure cartaoPostagemERP.SetdatajVigenciaInicio(Index: Integer; const AInteger: Integer);
begin
  FdatajVigenciaInicio := AInteger;
  FdatajVigenciaInicio_Specified := True;
end;

function cartaoPostagemERP.datajVigenciaInicio_Specified(Index: Integer): boolean;
begin
  Result := FdatajVigenciaInicio_Specified;
end;

procedure cartaoPostagemERP.SetdescricaoStatusCartao(Index: Integer; const Astring: string);
begin
  FdescricaoStatusCartao := Astring;
  FdescricaoStatusCartao_Specified := True;
end;

function cartaoPostagemERP.descricaoStatusCartao_Specified(Index: Integer): boolean;
begin
  Result := FdescricaoStatusCartao_Specified;
end;

procedure cartaoPostagemERP.SetdescricaoUnidadePostagemGenerica(Index: Integer; const Astring: string);
begin
  FdescricaoUnidadePostagemGenerica := Astring;
  FdescricaoUnidadePostagemGenerica_Specified := True;
end;

function cartaoPostagemERP.descricaoUnidadePostagemGenerica_Specified(Index: Integer): boolean;
begin
  Result := FdescricaoUnidadePostagemGenerica_Specified;
end;

procedure cartaoPostagemERP.SethorajAtualizacao(Index: Integer; const AInteger: Integer);
begin
  FhorajAtualizacao := AInteger;
  FhorajAtualizacao_Specified := True;
end;

function cartaoPostagemERP.horajAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FhorajAtualizacao_Specified;
end;

procedure cartaoPostagemERP.Setnumero(Index: Integer; const Astring: string);
begin
  Fnumero := Astring;
  Fnumero_Specified := True;
end;

function cartaoPostagemERP.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

procedure cartaoPostagemERP.Setservicos(Index: Integer; const AbuscaServicosResponse: buscaServicosResponse);
begin
  Fservicos := AbuscaServicosResponse;
  Fservicos_Specified := True;
end;

function cartaoPostagemERP.servicos_Specified(Index: Integer): boolean;
begin
  Result := Fservicos_Specified;
end;

procedure cartaoPostagemERP.SetstatusCartaoPostagem(Index: Integer; const Astring: string);
begin
  FstatusCartaoPostagem := Astring;
  FstatusCartaoPostagem_Specified := True;
end;

function cartaoPostagemERP.statusCartaoPostagem_Specified(Index: Integer): boolean;
begin
  Result := FstatusCartaoPostagem_Specified;
end;

procedure cartaoPostagemERP.SetstatusCodigo(Index: Integer; const Astring: string);
begin
  FstatusCodigo := Astring;
  FstatusCodigo_Specified := True;
end;

function cartaoPostagemERP.statusCodigo_Specified(Index: Integer): boolean;
begin
  Result := FstatusCodigo_Specified;
end;

procedure cartaoPostagemERP.SetunidadeGenerica(Index: Integer; const Astring: string);
begin
  FunidadeGenerica := Astring;
  FunidadeGenerica_Specified := True;
end;

function cartaoPostagemERP.unidadeGenerica_Specified(Index: Integer): boolean;
begin
  Result := FunidadeGenerica_Specified;
end;

procedure cartaoPostagemERP.SetunidadesPostagem(Index: Integer; const AArray_Of_unidadePostagemERP: Array_Of_unidadePostagemERP);
begin
  FunidadesPostagem := AArray_Of_unidadePostagemERP;
  FunidadesPostagem_Specified := True;
end;

function cartaoPostagemERP.unidadesPostagem_Specified(Index: Integer): boolean;
begin
  Result := FunidadesPostagem_Specified;
end;

procedure motivoPIMaster.Setcodigo(Index: Integer; const AInteger: Integer);
begin
  Fcodigo := AInteger;
  Fcodigo_Specified := True;
end;

function motivoPIMaster.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure motivoPIMaster.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function motivoPIMaster.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

destructor objetoPostal.Destroy;
begin
  System.SysUtils.FreeAndNil(FdataAtualizacaoCliente);
  System.SysUtils.FreeAndNil(FdataCancelamentoEtiqueta);
  System.SysUtils.FreeAndNil(FdataInclusao);
  System.SysUtils.FreeAndNil(FobjetoPostalPK);
  System.SysUtils.FreeAndNil(FpreListaPostagem);
  inherited Destroy;
end;

procedure objetoPostal.SetcodigoEtiqueta(Index: Integer; const Astring: string);
begin
  FcodigoEtiqueta := Astring;
  FcodigoEtiqueta_Specified := True;
end;

function objetoPostal.codigoEtiqueta_Specified(Index: Integer): boolean;
begin
  Result := FcodigoEtiqueta_Specified;
end;

procedure objetoPostal.SetdataAtualizacaoCliente(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacaoCliente := ATXSDateTime;
  FdataAtualizacaoCliente_Specified := True;
end;

function objetoPostal.dataAtualizacaoCliente_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacaoCliente_Specified;
end;

procedure objetoPostal.SetdataCancelamentoEtiqueta(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataCancelamentoEtiqueta := ATXSDateTime;
  FdataCancelamentoEtiqueta_Specified := True;
end;

function objetoPostal.dataCancelamentoEtiqueta_Specified(Index: Integer): boolean;
begin
  Result := FdataCancelamentoEtiqueta_Specified;
end;

procedure objetoPostal.SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataInclusao := ATXSDateTime;
  FdataInclusao_Specified := True;
end;

function objetoPostal.dataInclusao_Specified(Index: Integer): boolean;
begin
  Result := FdataInclusao_Specified;
end;

procedure objetoPostal.SetobjetoPostalPK(Index: Integer; const AobjetoPostalPK: objetoPostalPK);
begin
  FobjetoPostalPK := AobjetoPostalPK;
  FobjetoPostalPK_Specified := True;
end;

function objetoPostal.objetoPostalPK_Specified(Index: Integer): boolean;
begin
  Result := FobjetoPostalPK_Specified;
end;

procedure objetoPostal.SetplpNu(Index: Integer; const AInt64: Int64);
begin
  FplpNu := AInt64;
  FplpNu_Specified := True;
end;

function objetoPostal.plpNu_Specified(Index: Integer): boolean;
begin
  Result := FplpNu_Specified;
end;

procedure objetoPostal.SetpreListaPostagem(Index: Integer; const ApreListaPostagem: preListaPostagem);
begin
  FpreListaPostagem := ApreListaPostagem;
  FpreListaPostagem_Specified := True;
end;

function objetoPostal.preListaPostagem_Specified(Index: Integer): boolean;
begin
  Result := FpreListaPostagem_Specified;
end;

procedure objetoPostal.SetstatusEtiqueta(Index: Integer; const AstatusObjetoPostal: statusObjetoPostal);
begin
  FstatusEtiqueta := AstatusObjetoPostal;
  FstatusEtiqueta_Specified := True;
end;

function objetoPostal.statusEtiqueta_Specified(Index: Integer): boolean;
begin
  Result := FstatusEtiqueta_Specified;
end;

destructor usuarioInstalacao.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(Fparametros)-1 do
    System.SysUtils.FreeAndNil(Fparametros[I]);
  System.SetLength(Fparametros, 0);
  System.SysUtils.FreeAndNil(FdataAtualizacao);
  System.SysUtils.FreeAndNil(FdataInclusao);
  System.SysUtils.FreeAndNil(FdataSenha);
  System.SysUtils.FreeAndNil(FgerenteMaster);
  inherited Destroy;
end;

procedure usuarioInstalacao.SetdataAtualizacao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacao := ATXSDateTime;
  FdataAtualizacao_Specified := True;
end;

function usuarioInstalacao.dataAtualizacao_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacao_Specified;
end;

procedure usuarioInstalacao.SetdataInclusao(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataInclusao := ATXSDateTime;
  FdataInclusao_Specified := True;
end;

function usuarioInstalacao.dataInclusao_Specified(Index: Integer): boolean;
begin
  Result := FdataInclusao_Specified;
end;

procedure usuarioInstalacao.SetdataSenha(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataSenha := ATXSDateTime;
  FdataSenha_Specified := True;
end;

function usuarioInstalacao.dataSenha_Specified(Index: Integer): boolean;
begin
  Result := FdataSenha_Specified;
end;

procedure usuarioInstalacao.SetgerenteMaster(Index: Integer; const AgerenteConta: gerenteConta);
begin
  FgerenteMaster := AgerenteConta;
  FgerenteMaster_Specified := True;
end;

function usuarioInstalacao.gerenteMaster_Specified(Index: Integer): boolean;
begin
  Result := FgerenteMaster_Specified;
end;

procedure usuarioInstalacao.SethashSenha(Index: Integer; const Astring: string);
begin
  FhashSenha := Astring;
  FhashSenha_Specified := True;
end;

function usuarioInstalacao.hashSenha_Specified(Index: Integer): boolean;
begin
  Result := FhashSenha_Specified;
end;

procedure usuarioInstalacao.Setid(Index: Integer; const AInt64: Int64);
begin
  Fid := AInt64;
  Fid_Specified := True;
end;

function usuarioInstalacao.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure usuarioInstalacao.Setlogin(Index: Integer; const Astring: string);
begin
  Flogin := Astring;
  Flogin_Specified := True;
end;

function usuarioInstalacao.login_Specified(Index: Integer): boolean;
begin
  Result := Flogin_Specified;
end;

procedure usuarioInstalacao.Setnome(Index: Integer; const Astring: string);
begin
  Fnome := Astring;
  Fnome_Specified := True;
end;

function usuarioInstalacao.nome_Specified(Index: Integer): boolean;
begin
  Result := Fnome_Specified;
end;

procedure usuarioInstalacao.Setparametros(Index: Integer; const AArray_Of_parametroMaster: Array_Of_parametroMaster);
begin
  Fparametros := AArray_Of_parametroMaster;
  Fparametros_Specified := True;
end;

function usuarioInstalacao.parametros_Specified(Index: Integer): boolean;
begin
  Result := Fparametros_Specified;
end;

procedure usuarioInstalacao.Setsenha(Index: Integer; const Astring: string);
begin
  Fsenha := Astring;
  Fsenha_Specified := True;
end;

function usuarioInstalacao.senha_Specified(Index: Integer): boolean;
begin
  Result := Fsenha_Specified;
end;

procedure usuarioInstalacao.Setstatus(Index: Integer; const AstatusUsuario: statusUsuario);
begin
  Fstatus := AstatusUsuario;
  Fstatus_Specified := True;
end;

function usuarioInstalacao.status_Specified(Index: Integer): boolean;
begin
  Result := Fstatus_Specified;
end;

procedure usuarioInstalacao.Setvalidade(Index: Integer; const Astring: string);
begin
  Fvalidade := Astring;
  Fvalidade_Specified := True;
end;

function usuarioInstalacao.validade_Specified(Index: Integer): boolean;
begin
  Result := Fvalidade_Specified;
end;

destructor preListaPostagem.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FobjetosPostais)-1 do
    System.SysUtils.FreeAndNil(FobjetosPostais[I]);
  System.SetLength(FobjetosPostais, 0);
  System.SysUtils.FreeAndNil(FcartaoPostagem);
  System.SysUtils.FreeAndNil(FdataAtualizacaoCliente);
  System.SysUtils.FreeAndNil(FdataAtualizacaoSara);
  System.SysUtils.FreeAndNil(FdataFechamento);
  System.SysUtils.FreeAndNil(FdataPostagem);
  System.SysUtils.FreeAndNil(FdataPostagemSara);
  inherited Destroy;
end;

procedure preListaPostagem.SetcartaoPostagem(Index: Integer; const AcartaoPostagemERP: cartaoPostagemERP);
begin
  FcartaoPostagem := AcartaoPostagemERP;
  FcartaoPostagem_Specified := True;
end;

function preListaPostagem.cartaoPostagem_Specified(Index: Integer): boolean;
begin
  Result := FcartaoPostagem_Specified;
end;

procedure preListaPostagem.SetdataAtualizacaoCliente(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacaoCliente := ATXSDateTime;
  FdataAtualizacaoCliente_Specified := True;
end;

function preListaPostagem.dataAtualizacaoCliente_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacaoCliente_Specified;
end;

procedure preListaPostagem.SetdataAtualizacaoSara(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataAtualizacaoSara := ATXSDateTime;
  FdataAtualizacaoSara_Specified := True;
end;

function preListaPostagem.dataAtualizacaoSara_Specified(Index: Integer): boolean;
begin
  Result := FdataAtualizacaoSara_Specified;
end;

procedure preListaPostagem.SetdataFechamento(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataFechamento := ATXSDateTime;
  FdataFechamento_Specified := True;
end;

function preListaPostagem.dataFechamento_Specified(Index: Integer): boolean;
begin
  Result := FdataFechamento_Specified;
end;

procedure preListaPostagem.SetdataPostagem(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataPostagem := ATXSDateTime;
  FdataPostagem_Specified := True;
end;

function preListaPostagem.dataPostagem_Specified(Index: Integer): boolean;
begin
  Result := FdataPostagem_Specified;
end;

procedure preListaPostagem.SetdataPostagemSara(Index: Integer; const ATXSDateTime: TXSDateTime);
begin
  FdataPostagemSara := ATXSDateTime;
  FdataPostagemSara_Specified := True;
end;

function preListaPostagem.dataPostagemSara_Specified(Index: Integer): boolean;
begin
  Result := FdataPostagemSara_Specified;
end;

procedure preListaPostagem.SetobjetosPostais(Index: Integer; const AArray_Of_objetoPostal: Array_Of_objetoPostal);
begin
  FobjetosPostais := AArray_Of_objetoPostal;
  FobjetosPostais_Specified := True;
end;

function preListaPostagem.objetosPostais_Specified(Index: Integer): boolean;
begin
  Result := FobjetosPostais_Specified;
end;

procedure preListaPostagem.SetplpXml(Index: Integer; const AArray_Of_unsignedShort: Array_Of_unsignedShort);
begin
  FplpXml := AArray_Of_unsignedShort;
  FplpXml_Specified := True;
end;

function preListaPostagem.plpXml_Specified(Index: Integer): boolean;
begin
  Result := FplpXml_Specified;
end;

procedure preListaPostagem.SetplpXmlRetorno(Index: Integer; const AArray_Of_unsignedShort: Array_Of_unsignedShort);
begin
  FplpXmlRetorno := AArray_Of_unsignedShort;
  FplpXmlRetorno_Specified := True;
end;

function preListaPostagem.plpXmlRetorno_Specified(Index: Integer): boolean;
begin
  Result := FplpXmlRetorno_Specified;
end;

procedure preListaPostagem.Setstatus(Index: Integer; const AstatusPlp: statusPlp);
begin
  Fstatus := AstatusPlp;
  Fstatus_Specified := True;
end;

function preListaPostagem.status_Specified(Index: Integer): boolean;
begin
  Result := Fstatus_Specified;
end;

procedure objetoPostalPK.SetcodigoEtiqueta(Index: Integer; const Astring: string);
begin
  FcodigoEtiqueta := Astring;
  FcodigoEtiqueta_Specified := True;
end;

function objetoPostalPK.codigoEtiqueta_Specified(Index: Integer): boolean;
begin
  Result := FcodigoEtiqueta_Specified;
end;

procedure objetoTO.Setdesc(Index: Integer; const Astring: string);
begin
  Fdesc := Astring;
  Fdesc_Specified := True;
end;

function objetoTO.desc_Specified(Index: Integer): boolean;
begin
  Result := Fdesc_Specified;
end;

procedure objetoTO.Setentrega(Index: Integer; const Astring: string);
begin
  Fentrega := Astring;
  Fentrega_Specified := True;
end;

function objetoTO.entrega_Specified(Index: Integer): boolean;
begin
  Result := Fentrega_Specified;
end;

procedure objetoTO.Setid(Index: Integer; const Astring: string);
begin
  Fid := Astring;
  Fid_Specified := True;
end;

function objetoTO.id_Specified(Index: Integer): boolean;
begin
  Result := Fid_Specified;
end;

procedure objetoTO.Setitem(Index: Integer; const Astring: string);
begin
  Fitem := Astring;
  Fitem_Specified := True;
end;

function objetoTO.item_Specified(Index: Integer): boolean;
begin
  Result := Fitem_Specified;
end;

procedure objetoTO.Setnum(Index: Integer; const Astring: string);
begin
  Fnum := Astring;
  Fnum_Specified := True;
end;

function objetoTO.num_Specified(Index: Integer): boolean;
begin
  Result := Fnum_Specified;
end;

destructor coletaTO.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(Fproduto)-1 do
    System.SysUtils.FreeAndNil(Fproduto[I]);
  System.SetLength(Fproduto, 0);
  System.SysUtils.FreeAndNil(Fremetente);
  inherited Destroy;
end;

procedure coletaTO.Setcklist(Index: Integer; const Astring: string);
begin
  Fcklist := Astring;
  Fcklist_Specified := True;
end;

function coletaTO.cklist_Specified(Index: Integer): boolean;
begin
  Result := Fcklist_Specified;
end;

procedure coletaTO.Setdescricao(Index: Integer; const Astring: string);
begin
  Fdescricao := Astring;
  Fdescricao_Specified := True;
end;

function coletaTO.descricao_Specified(Index: Integer): boolean;
begin
  Result := Fdescricao_Specified;
end;

procedure coletaTO.Setid_cliente(Index: Integer; const Astring: string);
begin
  Fid_cliente := Astring;
  Fid_cliente_Specified := True;
end;

function coletaTO.id_cliente_Specified(Index: Integer): boolean;
begin
  Result := Fid_cliente_Specified;
end;

procedure coletaTO.Setproduto(Index: Integer; const AArray_Of_produtoTO: Array_Of_produtoTO);
begin
  Fproduto := AArray_Of_produtoTO;
  Fproduto_Specified := True;
end;

function coletaTO.produto_Specified(Index: Integer): boolean;
begin
  Result := Fproduto_Specified;
end;

procedure coletaTO.Setremetente(Index: Integer; const AremetenteTO: remetenteTO);
begin
  Fremetente := AremetenteTO;
  Fremetente_Specified := True;
end;

function coletaTO.remetente_Specified(Index: Integer): boolean;
begin
  Result := Fremetente_Specified;
end;

procedure coletaTO.Settipo(Index: Integer; const Astring: string);
begin
  Ftipo := Astring;
  Ftipo_Specified := True;
end;

function coletaTO.tipo_Specified(Index: Integer): boolean;
begin
  Result := Ftipo_Specified;
end;

procedure coletaTO.Setvalor_declarado(Index: Integer; const Astring: string);
begin
  Fvalor_declarado := Astring;
  Fvalor_declarado_Specified := True;
end;

function coletaTO.valor_declarado_Specified(Index: Integer): boolean;
begin
  Result := Fvalor_declarado_Specified;
end;

destructor coletaReversaTO.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(Fobj_col)-1 do
    System.SysUtils.FreeAndNil(Fobj_col[I]);
  System.SetLength(Fobj_col, 0);
  inherited Destroy;
end;

procedure coletaReversaTO.Setag(Index: Integer; const Astring: string);
begin
  Fag := Astring;
  Fag_Specified := True;
end;

function coletaReversaTO.ag_Specified(Index: Integer): boolean;
begin
  Result := Fag_Specified;
end;

procedure coletaReversaTO.Setar(Index: Integer; const AInteger: Integer);
begin
  Far := AInteger;
  Far_Specified := True;
end;

function coletaReversaTO.ar_Specified(Index: Integer): boolean;
begin
  Result := Far_Specified;
end;

procedure coletaReversaTO.Setcartao(Index: Integer; const AInt64: Int64);
begin
  Fcartao := AInt64;
  Fcartao_Specified := True;
end;

function coletaReversaTO.cartao_Specified(Index: Integer): boolean;
begin
  Result := Fcartao_Specified;
end;

procedure coletaReversaTO.Setnumero(Index: Integer; const AInteger: Integer);
begin
  Fnumero := AInteger;
  Fnumero_Specified := True;
end;

function coletaReversaTO.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

procedure coletaReversaTO.Setobj_col(Index: Integer; const AArray_Of_objetoTO: Array_Of_objetoTO);
begin
  Fobj_col := AArray_Of_objetoTO;
  Fobj_col_Specified := True;
end;

function coletaReversaTO.obj_col_Specified(Index: Integer): boolean;
begin
  Result := Fobj_col_Specified;
end;

procedure coletaReversaTO.Setservico_adicional(Index: Integer; const Astring: string);
begin
  Fservico_adicional := Astring;
  Fservico_adicional_Specified := True;
end;

function coletaReversaTO.servico_adicional_Specified(Index: Integer): boolean;
begin
  Result := Fservico_adicional_Specified;
end;

procedure coletaSimultaneaTO.Setobj(Index: Integer; const Astring: string);
begin
  Fobj := Astring;
  Fobj_Specified := True;
end;

function coletaSimultaneaTO.obj_Specified(Index: Integer): boolean;
begin
  Result := Fobj_Specified;
end;

procedure coletaSimultaneaTO.Setobs(Index: Integer; const Astring: string);
begin
  Fobs := Astring;
  Fobs_Specified := True;
end;

function coletaSimultaneaTO.obs_Specified(Index: Integer): boolean;
begin
  Result := Fobs_Specified;
end;

procedure pedidoInformacaoConsulta.Setnumero(Index: Integer; const AInt64: Int64);
begin
  Fnumero := AInt64;
  Fnumero_Specified := True;
end;

function pedidoInformacaoConsulta.numero_Specified(Index: Integer): boolean;
begin
  Result := Fnumero_Specified;
end;

procedure mensagemRetornoPIMaster.Setcodigo(Index: Integer; const AInteger: Integer);
begin
  Fcodigo := AInteger;
  Fcodigo_Specified := True;
end;

function mensagemRetornoPIMaster.codigo_Specified(Index: Integer): boolean;
begin
  Result := Fcodigo_Specified;
end;

procedure mensagemRetornoPIMaster.Setmensagem(Index: Integer; const Astring: string);
begin
  Fmensagem := Astring;
  Fmensagem_Specified := True;
end;

function mensagemRetornoPIMaster.mensagem_Specified(Index: Integer): boolean;
begin
  Result := Fmensagem_Specified;
end;

initialization
  { AtendeCliente }
  InvRegistry.RegisterInterface(TypeInfo(AtendeCliente), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'UTF-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(AtendeCliente), '');
  InvRegistry.RegisterInvokeOptions(TypeInfo(AtendeCliente), ioDocument);
  { AtendeCliente.fechaPlp }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'fechaPlp', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'xml', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'idPlpCliente', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'cartaoPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'faixaEtiquetas', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlp', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.registrarPedidosInformacao }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'registrarPedidosInformacao', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'registrarPedidosInformacao', 'pedidosInformacao', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'registrarPedidosInformacao', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'registrarPedidosInformacao', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'registrarPedidosInformacao', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.buscaCliente }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'buscaCliente', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaCliente', 'idContrato', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaCliente', 'idCartaoPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaCliente', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaCliente', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaCliente', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.validaEtiquetaPLP }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', '',
                                 '[ReturnName="return"]', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', 'numeroEtiqueta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', 'idPlp', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaEtiquetaPLP', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.verificaDisponibilidadeServico }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', '',
                                 '[ReturnName="return"]', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'numeroServico', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'cepOrigem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'cepDestino', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'verificaDisponibilidadeServico', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.getStatusPLP }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'getStatusPLP', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusPLP', 'arg0', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusPLP', 'arg1', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusPLP', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.bloquearObjeto }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'numeroEtiqueta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'idPlp', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'tipoBloqueio', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'acao', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'bloquearObjeto', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.solicitaEtiquetas }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'tipoDestinatario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'identificador', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'idServico', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'qtdEtiquetas', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaEtiquetas', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.obterMensagemRetornoPI }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'obterMensagemRetornoPI', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterMensagemRetornoPI', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.consultarPedidosInformacao }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'consultarPedidosInformacao', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultarPedidosInformacao', 'pedidosInformacao', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultarPedidosInformacao', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultarPedidosInformacao', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultarPedidosInformacao', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.buscaPagamentoEntrega }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'buscaPagamentoEntrega', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaPagamentoEntrega', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaPagamentoEntrega', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaPagamentoEntrega', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.geraDigitoVerificadorEtiquetas }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'geraDigitoVerificadorEtiquetas', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'geraDigitoVerificadorEtiquetas', 'etiquetas', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'geraDigitoVerificadorEtiquetas', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'geraDigitoVerificadorEtiquetas', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'geraDigitoVerificadorEtiquetas', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.validarPostagemReversa }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'codigoServico', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'cepDestinatario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'coleta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemReversa', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.fechaPlpVariosServicos }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'xml', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'idPlpCliente', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'cartaoPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'listaEtiquetas', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'fechaPlpVariosServicos', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.cancelarObjeto }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', 'idPlp', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', 'numeroEtiqueta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarObjeto', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.validaPlp }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'validaPlp', '',
                                 '[ReturnName="return"]', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'cliente', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'numero', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'diretoria', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'cartao', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'unidadePostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'servico', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'servicosAdicionais', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validaPlp', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.validarPostagemSimultanea }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'codigoServico', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'cepDestinatario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'coleta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'validarPostagemSimultanea', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.obterEmbalagemLRS }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'obterEmbalagemLRS', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterEmbalagemLRS', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.cancelarPedidoScol }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'idPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'tipo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'cancelarPedidoScol', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.buscaServicos }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'buscaServicos', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaServicos', 'idContrato', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaServicos', 'idCartaoPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaServicos', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaServicos', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaServicos', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.solicitarPostagemScol }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', 'xml', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitarPostagemScol', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.solicitaPLP }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'solicitaPLP', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaPLP', 'idPlpMaster', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaPLP', 'numEtiqueta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaPLP', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaPLP', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaPLP', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.getStatusCartaoPostagem }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'getStatusCartaoPostagem', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusCartaoPostagem', 'numeroCartaoPostagem', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusCartaoPostagem', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusCartaoPostagem', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'getStatusCartaoPostagem', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.solicitaXmlPlp }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'solicitaXmlPlp', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaXmlPlp', 'idPlpMaster', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaXmlPlp', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaXmlPlp', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'solicitaXmlPlp', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.obterMotivosPI }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'obterMotivosPI', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterMotivosPI', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.buscaContrato }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'buscaContrato', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaContrato', 'numero', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaContrato', 'diretoria', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaContrato', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaContrato', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'buscaContrato', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.consultaSRO }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'consultaSRO', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'listaObjetos', '',
                                '', IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'tipoConsulta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'tipoResultado', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'usuarioSro', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'senhaSro', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaSRO', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.obterClienteAtualizacao }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'obterClienteAtualizacao', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterClienteAtualizacao', 'cnpjCliente', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterClienteAtualizacao', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterClienteAtualizacao', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterClienteAtualizacao', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.integrarUsuarioScol }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'integrarUsuarioScol', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'integrarUsuarioScol', 'codAdministrativo', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'integrarUsuarioScol', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'integrarUsuarioScol', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'integrarUsuarioScol', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.atualizaPLP }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'atualizaPLP', '',
                                 '[ReturnName="return"]', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'idPlpMaster', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'numEtiqueta', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'usuario', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'senha', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'xml', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'atualizaPLP', 'return', '',
                                '', IS_UNQL);
  { AtendeCliente.obterAssuntosPI }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'obterAssuntosPI', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNBD or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'obterAssuntosPI', 'return', '',
                                '[ArrayItemName="return"]', IS_UNBD or IS_UNQL);
  { AtendeCliente.consultaCEP }
  InvRegistry.RegisterMethodInfo(TypeInfo(AtendeCliente), 'consultaCEP', '',
                                 '[ReturnName="return"]', IS_OPTN or IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaCEP', 'cep', '',
                                '', IS_UNQL);
  InvRegistry.RegisterParamInfo(TypeInfo(AtendeCliente), 'consultaCEP', 'return', '',
                                '', IS_UNQL);
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_parametroMaster), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_parametroMaster');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_servicoSigep), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_servicoSigep');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_objetoPostal), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_objetoPostal');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_objetoTO), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_objetoTO');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_produtoTO), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_produtoTO');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_pedidoInformacaoConsulta), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_pedidoInformacaoConsulta');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_servicoAdicionalERP), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_servicoAdicionalERP');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_pedidoInformacaoRegistro), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_pedidoInformacaoRegistro');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_contratoERP), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_contratoERP');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_usuarioInstalacao), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_usuarioInstalacao');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_unidadePostagemERP), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_unidadePostagemERP');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_clienteERP), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_clienteERP');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_gerenteConta), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_gerenteConta');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_cartaoPostagemERP), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Array_Of_cartaoPostagemERP');
  RemClassRegistry.RegisterXSInfo(TypeInfo(statusGerente), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'statusGerente');
  RemClassRegistry.RegisterXSInfo(TypeInfo(tipoGerente), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'tipoGerente');
  RemClassRegistry.RegisterXSInfo(TypeInfo(categoriaServico), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'categoriaServico');
  RemClassRegistry.RegisterXSClass(cliente, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'cliente');
  RemClassRegistry.RegisterXSClass(conta, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'conta');
  RemClassRegistry.RegisterXSClass(enderecoERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'enderecoERP');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(enderecoERP), 'end_', '[ExtName="end"]');
  RemClassRegistry.RegisterXSClass(contratoERPPK, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'contratoERPPK');
  RemClassRegistry.RegisterXSClass(unidadePostagemERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'unidadePostagemERP');
  RemClassRegistry.RegisterXSClass(servicoSigep, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'servicoSigep');
  RemClassRegistry.RegisterXSClass(SQLException, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'SQLException');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(SQLException), 'message_', '[ExtName="message"]');
  RemClassRegistry.RegisterXSClass(Exception, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Exception');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Exception), 'message_', '[ExtName="message"]');
  RemClassRegistry.RegisterXSClass(AutenticacaoException, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'AutenticacaoException');
  RemClassRegistry.RegisterSerializeOptions(AutenticacaoException, [xoSimpleTypeWrapper]);
  RemClassRegistry.RegisterXSClass(SigepClienteException, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'SigepClienteException');
  RemClassRegistry.RegisterSerializeOptions(SigepClienteException, [xoSimpleTypeWrapper]);
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_string), 'http://www.w3.org/2001/XMLSchema', 'Array_Of_string');
  RemClassRegistry.RegisterXSClass(pedidoInformacao, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'pedidoInformacao');
  RemClassRegistry.RegisterXSClass(pedidoInformacaoRegistro, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'pedidoInformacaoRegistro');
  RemClassRegistry.RegisterXSClass(retorno, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'retorno');
  RemClassRegistry.RegisterXSClass(clienteERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'clienteERP');
  RemClassRegistry.RegisterXSClass(chancelaMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'chancelaMaster');
  RemClassRegistry.RegisterXSClass(gerenteConta, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'gerenteConta');
  RemClassRegistry.RegisterXSClass(servicoAdicionalERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'servicoAdicionalERP');
  RemClassRegistry.RegisterXSClass(vigenciaERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'vigenciaERP');
  RemClassRegistry.RegisterXSClass(servicoERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'servicoERP');
  RemClassRegistry.RegisterXSClass(contratoERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'contratoERP');
  RemClassRegistry.RegisterXSClass(postagem, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'postagem');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(postagem), 'local_', '[ExtName="local"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(registrarPedidosInformacaoResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'registrarPedidosInformacaoResponse');
  RemClassRegistry.RegisterXSClass(pessoa, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'pessoa');
  RemClassRegistry.RegisterXSClass(remetente, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'remetente');
  RemClassRegistry.RegisterXSClass(destinatario, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'destinatario');
  RemClassRegistry.RegisterXSClass(parametroMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'parametroMaster');
  RemClassRegistry.RegisterXSClass(embalagemLRSMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'embalagemLRSMaster');
  RemClassRegistry.RegisterXSInfo(TypeInfo(obterEmbalagemLRSResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'obterEmbalagemLRSResponse');
  RemClassRegistry.RegisterXSClass(objetoSimplificadoTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'objetoSimplificadoTO');
  RemClassRegistry.RegisterXSClass(retornoCancelamentoTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'retornoCancelamentoTO');
  RemClassRegistry.RegisterXSClass(produtoTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'produtoTO');
  RemClassRegistry.RegisterXSClass(pessoaTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'pessoaTO');
  RemClassRegistry.RegisterXSClass(remetenteTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'remetenteTO');
  RemClassRegistry.RegisterXSClass(assuntoPIMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'assuntoPIMaster');
  RemClassRegistry.RegisterXSInfo(TypeInfo(obterAssuntosPIResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'obterAssuntosPIResponse');
  RemClassRegistry.RegisterXSClass(SQLException2, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'SQLException2', 'SQLException');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(SQLException2), 'message_', '[ExtName="message"]');
  RemClassRegistry.RegisterXSClass(Exception2, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'Exception2', 'Exception');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(Exception2), 'message_', '[ExtName="message"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(statusCartao), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'statusCartao');
  RemClassRegistry.RegisterXSInfo(TypeInfo(buscaServicosResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'buscaServicosResponse');
  RemClassRegistry.RegisterXSClass(cartaoPostagemERP, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'cartaoPostagemERP');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(cartaoPostagemERP), 'servicos', '[ArrayItemName="return"]');
  RemClassRegistry.RegisterXSClass(motivoPIMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'motivoPIMaster');
  RemClassRegistry.RegisterXSInfo(TypeInfo(obterMotivosPIResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'obterMotivosPIResponse');
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_unsignedShort), 'http://www.w3.org/2001/XMLSchema', 'Array_Of_unsignedShort');
  RemClassRegistry.RegisterXSInfo(TypeInfo(statusObjetoPostal), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'statusObjetoPostal');
  RemClassRegistry.RegisterXSInfo(TypeInfo(tipoBloqueio), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'tipoBloqueio');
  RemClassRegistry.RegisterXSInfo(TypeInfo(statusPlp), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'statusPlp');
  RemClassRegistry.RegisterXSClass(objetoPostal, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'objetoPostal');
  RemClassRegistry.RegisterXSInfo(TypeInfo(statusUsuario), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'statusUsuario');
  RemClassRegistry.RegisterXSClass(usuarioInstalacao, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'usuarioInstalacao');
  RemClassRegistry.RegisterXSClass(preListaPostagem, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'preListaPostagem');
  RemClassRegistry.RegisterXSClass(objetoPostalPK, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'objetoPostalPK');
  RemClassRegistry.RegisterXSInfo(TypeInfo(geraDigitoVerificadorEtiquetasResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'geraDigitoVerificadorEtiquetasResponse');
  RemClassRegistry.RegisterXSClass(objetoTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'objetoTO');
  RemClassRegistry.RegisterXSClass(coletaTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'coletaTO');
  RemClassRegistry.RegisterXSClass(coletaReversaTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'coletaReversaTO');
  RemClassRegistry.RegisterXSClass(coletaSimultaneaTO, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'coletaSimultaneaTO');
  RemClassRegistry.RegisterXSInfo(TypeInfo(obterMensagemRetornoPIResponse), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'obterMensagemRetornoPIResponse');
  RemClassRegistry.RegisterXSInfo(TypeInfo(acao), 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'acao');
  RemClassRegistry.RegisterXSClass(pedidoInformacaoConsulta, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'pedidoInformacaoConsulta');
  RemClassRegistry.RegisterXSClass(mensagemRetornoPIMaster, 'http://cliente.bean.master.sigep.bsb.correios.com.br/', 'mensagemRetornoPIMaster');

end.