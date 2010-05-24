setlocal
set extpascalpath=..\..\
fpc myproject1.lpr -Fu%extpascalpath% -dUseRuntime -Smdcghie50 -venwi -l -Cirot -gl
