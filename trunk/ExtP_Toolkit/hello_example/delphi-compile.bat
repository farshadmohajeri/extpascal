setlocal
set delphipath=C:\Delphi7
set extpascalpath=..\..\
%delphipath%\bin\dcc32 myproject1.lpr /u%extpascalpath% /dUseRuntime /$b-,h+,j+,p+,q+,r+,t-,v-,x+
