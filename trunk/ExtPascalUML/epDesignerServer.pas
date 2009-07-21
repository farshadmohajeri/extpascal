{$I epDirectives.inc}

unit epDesignerServer;

interface

uses
  ExtPascalUMLModel;

procedure DisparaObjectServer(pGeracao : TGeracao);
procedure CopiaPacote(pOldPacote, pNovoPacote : TPacote);

implementation

uses
  SysUtils, StrUtils, Process, Classes, {$IFDEF MSWINDOWS}WinSvc, Services,{$ENDIF}
  epUtils, epPrevalence, epThread, epServer, epGenerator, epGDB;

function ExibirObjectServerLog(pPath, pNome : string) : boolean;
var
  Arq : Text;
  Lidos : integer;

  procedure OpenArq;
  var
    Ok : boolean;
    I : integer;
  begin
    filemode := $40;
    repeat
      try
        reset(Arq);
        Ok := true;
      except
        sleep(300);
        Ok := false;
      end;
    until Ok;
    for I := 1 to Lidos do readln(Arq);
  end;

  procedure LeLinha;
  var
    fLine : string;
  begin
    readln(Arq, fLine); inc(Lidos);
    if pos('<\END>', fLine) > 0 then
      Result := true
    else
      if fLine = '' then begin
        close(Arq);
        sleep(500);
        OpenArq;
      end
      else
        Browser.Message(fLine);
  end;

begin
  Result := false;
  Lidos  := 0;
  Browser.Message('Log do ExtPascalUML...');
  sleep(500);
  if FileExists(pPath + PathDelim + pNome + '.trc') then begin
    assign(Arq, pPath + PathDelim + pNome + '.trc');
    OpenArq;
    sleep(500);
    while not eof(Arq) and not Result do LeLinha;
    close(Arq);
    RenameFile(pPath + PathDelim + pNome + '.trc', pPath + PathDelim + pNome + '_Safe.trc');
  end;
end;

function DispararServico(pPath, pNome, pServico : string) : boolean;
var
  Error : integer;
{$IFDEF MSWINDOWS}
  Service : TService;
begin
  Service := TService.Create(pServico);
  try
    Result := true;
    if not Service.Exists then
      Service.Insert(pPath + PathDelim + pNome + '.exe')
    else begin
      DesassociaServicoDesigner(pServico, false);
      sleep(500);
    end;
    Browser.Message('.Disparando o serviço...');
    Error := Service.Start;
    case Error of
      0    : ; //Browser.Message('.Serviço iniciado');
      1056 : ; // Já startado
      1058 : Service.Reset;
    else
      Browser.Message(SysErrorMessage(Error));
      Result := false;
      exit;
    end;
    Result := ExibirObjectServerLog(pPath, pNome);
    if Result then begin
      if Service.GetState = SERVICE_RUNNING then begin
        AssociaServicoDesigner(pServico);
        Browser.Message('.Serviço iniciado');
      end;
    end
    else begin
      Browser.Message('.Serviço falhou ao entrar');
      DesassociaServicoDesigner(pServico);
    end;
  finally
    Service.Free;
  end;
{$ELSE}
begin
  Result := false
{$ENDIF}
end;

function DispararServidor(pPath, pNome : string) : boolean; begin
(*  if Servidor <> nil then begin
    Servidor.Terminate(0);
    while Servidor.Running do sleep(100);
    Servidor.Free;
  end;
  Servidor := TProcess.Create(nil);
  with Servidor do begin
    CommandLine := pPath + PathDelim + pNome;
    Options := [poNewConsole, poStderrToOutPut];
    StartupOptions := [suoUseShowWindow]; ShowWindow := swoShowMaximized;
    Execute;
  end;
  Browser.Message('.Disparando o servidor...');
  Result := ExibirObjectServerLog(pPath, pNome);
  if Result then begin
    AssociaServicoDesigner(pNome);
    Browser.Message('.Servidor iniciado');
  end
  else begin
    Browser.Message('.Servidor falhou ao entrar');
    Servidor.Terminate(0);
    Servidor.Free;
    Servidor := nil;
  end;*)
  Result := true;
end;

procedure SelecionarObjeto(Path, S : string; pThread : TThread);
var
  I, J : integer;
begin
  write('^^^', S);//
  I := pos('.pas:', S);
  if I <> 0 then begin
    I := PosExR(' ', S, I)+1;
    J := PosEx(^M, S, I);
    //Browser.ChangeThread(TepThread(pThread));
    TPacote.SelecionarObjeto(SourceToModel(Path, trim(copy(S, I, J-I))));
    //Browser.RestoreThread;
  end;
end;

function DepurarServidor(pPathGDB, pPath, pNome, pSessao : string) : boolean; begin
  try
    Browser.GDB := TGDB.Create(pPathGDB, pPath + PathDelim + pNome, SelecionarObjeto, Browser);
    TGDB(Browser.GDB).Command('continue', false);
    Result := ExibirObjectServerLog(pPath, pNome);
    if Result then begin
      AssociaServicoDesigner(pSessao);
      Browser.Message('.Servidor iniciado');
    end
    else begin
      Browser.Message('.Servidor falhou ao entrar');
      FreeAndNil(Browser.GDB);
    end;
  except
    on E : Exception do begin
      Browser.Message(E.Message);
      FreeAndNil(Browser.GDB);
      Result := false;
    end;
  end;
end;

procedure GerarIniFile(pNome : String; pServidor : TServidor; Geracao : TGeracao);
var
  Parametro : TParametroInicializacao;
  SessaoAnt, Ambiente : String;
  FaltouDirectory : boolean;
  Arq : Text;
begin
  assign(Arq, pNome);
  rewrite(Arq);
  writeln(Arq, '[CONNECTION]');
  writeln(Arq, 'PORT=', Geracao.Porta);
  writeln(Arq, 'SERVER=127.0.0.1');(*// + GetComputerName);*)
  case Geracao.Ambiente of
    gaDesenvolvimento : Ambiente := 'DEVELOPMENT';
    gaTeste : Ambiente := 'TEST';
    gaHomologacao : Ambiente := 'QUALITY ASSURANCE';
  else
    Ambiente := 'PRODUCTION';
  end;
  FaltouDirectory := true;
  SessaoAnt := '';
  Parametro := pServidor.Inicializacao.First;
  while Parametro <> nil do
    with Parametro do begin
      if piaServer in Aplicavel then
        if Sessao <> 'CONNECTION' then begin
          if Sessao = 'DIRECTORY' then FaltouDirectory := false;
          if SessaoAnt <> Sessao then begin
            writeln(Arq, '[' + Sessao + ']');
            SessaoAnt := Sessao;
            if Sessao = 'SERVER' then begin
              writeln(Arq, 'ENVIRONMENT=', Ambiente);
              with Geracao.Modelo do
                writeln(Arq, 'TITLE=', IfThen(Apelido = '', Nome, Apelido));
              writeln(Arq, 'SECURITY=', IfThen(goSecurity in Geracao.Opcoes, 'TRUE', 'FALSE'));
            end;
          end;
          writeln(Arq, Nome, '=', Valor);
        end;
      pServidor.Inicializacao.Next(Parametro);
    end;
  if FaltouDirectory then begin
    writeln(Arq, '[DIRECTORY]');
    writeln(Arq, 'SNAPSHOT=');
    writeln(Arq, 'LOG=');
    writeln(Arq, 'BACKUP=');
  end;
  close(Arq);
end;

function DescobrePortaDisponivel(pModelo : String; pServidor : TServidor) : integer;
var
  lGeracao : TGeracao;
begin
  for Result := pServidor.PortaInicial to pServidor.PortaFinal do begin
    lGeracao := GeracaoPorPortaList.Find(Result);
    if (lGeracao = nil) or ((lGeracao.Usuario = Browser.UserInfo.UserName) and (lGeracao.Modelo.Nome = pModelo)) then exit;
  end;
  Result := 0;
end;

procedure DisparaObjectServer(pGeracao : TGeracao);
var
  fServidor : TServidor;
  fNome, fPath, FPCPath : String;
  Iniciou : boolean;
begin
  fServidor := ServidorList.First;
  fPath     := fServidor.RaizGeracao;
  if fPath[length(fPath)] <> PathDelim then fPath := fPath + PathDelim;
  //if fGeracao.Ambiente = gaDesenvolvimento then
    fPath := fPath + Browser.UserInfo.UserName + PathDelim;
  fPath := fPath + pGeracao.Modelo.Nome;
  if not DirectoryExists(fPath) then
    raise Exception.Create('Modelo ainda não gerado. Disparo abortado.');
  fNome := pGeracao.Modelo.Nome;
  pGeracao.Porta := DescobrePortaDisponivel(pGeracao.Modelo.Nome, fServidor);
  if pGeracao.Porta = 0 then
    raise Exception.Create('Não existe porta disponível para alocação. Procure o Administrador do Sistema.');
  GerarIniFile(fPath + PathDelim + fNome + '.ini', fServidor, pGeracao);
  if goDebugger in pGeracao.Opcoes then begin
    FPCPath := copy(fServidor.Versao.Path, 1, length(fServidor.Versao.Path)-3) + 'FreePascal';
    Iniciou := DepurarServidor(ExpandMacros(['$(FPCPATH)', FPCPath], ExtractFilePath(pGeracao.Compilador.Executavel)),
      fPath, fNome, fNome + Browser.UserInfo.UserName)
  end
  else
    if NoService then
      Iniciou := DispararServidor(fPath, fNome)
    else
      Iniciou := DispararServico(fPath, fNome, fNome + IfThen(pGeracao.Ambiente = gaDesenvolvimento, Browser.UserInfo.UserName, ''));
  if Iniciou then begin
    fNome := Browser.UserInfo.ComputerName + ' ' + intToStr(pGeracao.Porta);
    Browser.Message('Disparando Browser... ' + fNome);
    //Browser.ExecuteBrowser(fNome);
  end;
end;

procedure CopiaPacote(pOldPacote, pNovoPacote : TPacote);
var
  fNovoPacote : TPacote;
begin
  try
    BeginTransaction;
    fNovoPacote := TPacote(pOldPacote.CopyObjectCascade);
    fNovoPacote.Nome := 'Novo' + '_' + FormatDateTime('yyyymmdd_hhnnss', Now);
    EndTransaction;
  except
    Rollback;
  end;
end;
end.
