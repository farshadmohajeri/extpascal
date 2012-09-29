{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcgiapp_pkg;

interface

uses
  FCGIApp, BlockSocket, ExtPascalClasses, ExtPascalUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('fcgiapp_pkg', @Register);
end.
