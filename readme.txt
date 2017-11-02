
runmet_ctem.f90 is the main code, and depends on met_module.f90

to compile

#for double precision met file 

 gfortran -fconvert=big-endian -m64 -fdefault-real-8 ffree-form -o executable.exe runmet_ctem.f90 met_module.f90

# for single precision met file 
 gfortran -fconvert=big-endian -o executable.exe runmet_ctem.f90 met_module.f90 #model run successfully on Hadar


## when compiling using pgf90 compiler we use slightly different version of codes runmet_ctem_had.f90 and met_module_had.f90
## I declare rand intrinsic function in met_module_had.f90


# single precision met file 

 pgf90 -Mbyteswapio -o executable.exe runmet_ctem_had.f90 met_module_had.f90 ##this does not work on Hadar



