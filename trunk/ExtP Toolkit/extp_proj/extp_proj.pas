{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit ExtP_Proj; 

interface

uses
  ExtP_Proj_Intf, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ExtP_Proj_Intf', @ExtP_Proj_Intf.Register); 
end; 

initialization
  RegisterPackage('ExtP_Proj', @Register); 
end.
