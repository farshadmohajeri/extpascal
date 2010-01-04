{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit extp_ctrls; 

interface

uses
  ExtP_Design_Ctrls, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ExtP_Design_Ctrls', @ExtP_Design_Ctrls.Register); 
end; 

initialization
  RegisterPackage('ExtP_Ctrls', @Register); 
end.
