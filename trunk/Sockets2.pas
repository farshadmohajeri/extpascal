{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                                                                                           
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Sockets2; {$IFNDEF FPC}{$DEFINE FPC_BIG_ENDIAN}{$ENDIF}


Interface

Uses
  winsock2,ctypes;

Type
  size_t  = cuint32;
  ssize_t = cint32;
  tsocklen= cint;
  psocklen= ^tsocklen;

const
  EsockEINTR           = WSAEINTR;
  EsockEBADF           = WSAEBADF;
  EsockEFAULT          = WSAEFAULT;
  EsockEINVAL          = WSAEINVAL;
  EsockEACCESS         = WSAEACCES;
  EsockEMFILE          = WSAEMFILE;
  EsockEMSGSIZE        = WSAEMSGSIZE;
  EsockENOBUFS         = WSAENOBUFS;
  EsockENOTCONN        = WSAENOTCONN;
  EsockENOTSOCK        = WSAENOTSOCK;
  EsockEPROTONOSUPPORT = WSAEPROTONOSUPPORT;
  EsockEWOULDBLOCK     = WSAEWOULDBLOCK;

{$i socketsh.inc}
{$i fpwinsockh.inc}

Implementation

function socketerror:cint;
begin
 result:=wsagetlasterror;
end;

function fpsocket       (domain:cint; xtype:cint; protocol: cint):cint;
begin
  fpSocket:=WinSock2.Socket(Domain,xtype,ProtoCol);
end;

function fpsend (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
begin
  fpSend:=WinSock2.Send(S,msg,len,flags);
end;

function fpsendto (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
begin
  // Dubious construct, this should be checked. (IPV6 fails ?)
  fpSendTo:=WinSock2.SendTo(S,msg,Len,Flags,Winsock2.PSockAddr(tox),toLen);
end;

function fprecv         (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t;
begin
  fpRecv:=WinSock2.Recv(S,Buf,Len,Flags);
end;

function fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : plongint):ssize_t;

begin
  fpRecvFrom:=WinSock2.RecvFrom(S,Buf,Len,Flags,WinSock2.PSockAddr(From),FromLen);
end;

function fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;

begin
  fpConnect:=Winsock2.Connect(S,WinSock2.PSockAddr(name),nameLen);
end;

function fpshutdown     (s:cint; how:cint):cint;
begin
  fpShutDown:=Winsock2.ShutDown(S,How);
end;

Function socket(Domain,SocketType,Protocol:Longint):Longint;
begin
  socket:=fpsocket(Domain,sockettype,protocol);
end;

Function Send(Sock:Longint;Const Buf;BufLen,Flags:Longint):Longint;

begin
  send:=fpsend(sock,@buf,buflen,flags);
end;

Function SendTo(Sock:Longint;Const Buf;BufLen,Flags:Longint;Var Addr; AddrLen : Longint):Longint;

begin
  sendto:=fpsendto(sock,@buf,buflen,flags,@addr,addrlen);
end;

Function Recv(Sock:Longint;Var Buf;BufLen,Flags:Longint):Longint;
begin
  Recv:=fpRecv(Sock,@Buf,BufLen,Flags);
end;

Function RecvFrom(Sock : Longint; Var Buf; Buflen,Flags : Longint; Var Addr; var AddrLen : longint) : longint;
begin
  RecvFrom:=fpRecvFrom(Sock,@Buf,BufLen,Flags,@Addr,@AddrLen);
end;

function fpbind (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
begin
  fpbind:=Winsock2.Bind(S,Winsock2.PSockAddr(Addrx),AddrLen);
end;

function fplisten      (s:cint; backlog : cint):cint;
begin
  fplisten:=Winsock2.Listen(S,backlog);
end;

function fpaccept      (s:cint; addrx : psockaddr; addrlen : plongint):cint;
begin
  fpAccept:=Winsock2.Accept(S,Winsock2.PSockAddr(Addrx), AddrLen);
end;

function fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  fpGetSockName:=Winsock2.GetSockName(S,Winsock2.TSockAddr(name^),nameLen^);
end;

function fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  fpGetPeerName:=Winsock2.GetPeerName(S,Winsock2.TSockAddr(name^),NameLen^);
end;

function fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
begin
  fpGetSockOpt:=Winsock2.GetSockOpt(S,Level,OptName,OptVal,OptLen^);
end;

function fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
begin
  fpSetSockOpt:=Winsock2.SetSockOpt(S,Level,OptName,OptVal,OptLen);
end;

function fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint;
begin
  fpsocketpair:=-1;
  WSASetLastError(EOPNOTSUPP); // so that wsagetlasterror retrieves it
end;

Function CloseSocket(Sock:Longint):Longint;
begin
  result := Winsock2.CloseSocket (Sock);
end;

Function Bind(Sock:Longint;Const Addr;AddrLen:Longint):Boolean;

begin
  bind:=fpBind(Sock,@Addr,AddrLen)=0;
end;

Function Listen(Sock,MaxConnect:Longint):Boolean;

begin
  Listen:=fplisten(Sock,MaxConnect)=0;
end;

Function Accept(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;

begin
  Accept:=FPAccept(sock,@addr,@addrlen);
end;

Function Shutdown(Sock:Longint;How:Longint):Longint;

begin
 shutdown:=fpshutdown(sock,how);
end;

Function Connect(Sock:Longint;Const Addr;Addrlen:Longint):Boolean;

begin
 connect:=fpconnect(sock,@addr,addrlen)=0;
end;

Function GetSocketName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
 GetSocketName:=fpGetSockName(sock,@addr,@addrlen);
end;

Function GetPeerName(Sock:Longint;Var Addr;Var Addrlen:Longint):Longint;
begin
 GetPeerName:=fpGetPeerName(Sock,@addr,@addrlen);
end;

Function GetSocketOptions(Sock,Level,OptName:Longint;Var OptVal;Var optlen:longint):Longint;
begin
 GetSocketOptions:=fpGetSockOpt(sock,level,optname,@optval,@optlen);
end;

Function SetSocketOptions(Sock,Level,OptName:Longint;Const OptVal;optlen:longint):Longint;

begin
 SetSocketOptions:=fpsetsockopt(sock,level,optname,@optval,optlen);
end;

Function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
  SocketPair:=fpsocketpair(domain,sockettype,protocol,@pair[1]);
end;

{$i sockets.inc}

{ Winsocket stack needs an init. and cleanup code }
var
  wsadata : twsadata;

initialization
  WSAStartUp($2,wsadata);
finalization
  WSACleanUp;
end.
