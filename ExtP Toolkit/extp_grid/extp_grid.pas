{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit extp_grid; 

interface

uses
  ExtP_Design_Grid, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ExtP_Design_Grid', @ExtP_Design_Grid.Register); 
end; 

initialization
  RegisterPackage('ExtP_Grid', @Register); 
end.
