program runmet_ctem

use met_module 

implicit none 
       integer, parameter                                                 :: size_row = 1460*112    !365*4*112 6-h data for 112 years
       integer, parameter                                                 :: size_col = 7          !nos of variable without time and date  
       integer,parameter                                                  :: start_year = 1901  ! First met data year, if 
                                                                                                ! this is different change here
       integer,parameter                                                  :: total_year = 112   ! If this is different change here
       integer, parameter                                                 :: pi = 3.14159265
       integer, parameter                                                 :: quantity = 144
       integer,dimension(144)                                             :: hour1, minute1, day1
       integer                                                            :: i, j, k, day, lcltmind
       integer                                                            :: hour, minute, year, l, x, y, z, N  
       integer                                                            :: count, shift
       integer                                                            :: long_ind, lat_ind

       real                                                               :: long, lat
       real, dimension(13, size_col)                                      :: var
       real                                                               :: reallat 
       real                                                               :: reallong

       real, dimension(5, size_col)                                       :: par
       real, dimension(48)                                                :: SW_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: LW_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: PCP_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: T_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: H_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: W_interpolated_num_48_1
       real, dimension(:),allocatable                                     :: P_interpolated_num_48_1 

       real, dimension(48)                                                :: SW_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: LW_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: PCP_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: T_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: H_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: W_interpolated_num_48_2
       real, dimension(:),allocatable                                     :: P_interpolated_num_48_2

       real, dimension(48)                                                :: SW_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: LW_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: PCP_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: T_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: H_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: W_interpolated_num_48_3
       real, dimension(:),allocatable                                     :: P_interpolated_num_48_3 
    
       real, dimension(144)                                               :: SW_interpolated_num     
       real, dimension(144)                                               :: LW_interpolated_num
       real, dimension(144)                                               :: PCP_interpolated_num
       real, dimension(144)                                               :: T_interpolated_num
       real, dimension(144)                                               :: H_interpolated_num
       real, dimension(144)                                               :: W_interpolated_num
       real, dimension(144)                                               :: P_interpolated_num  

       real, dimension(144)                                               :: H_shifted_num,W_shifted_num,P_shifted_num
       real, dimension(144)                                               :: T_shifted_num,LW_shifted_num,PCP_shifted_num
   
       character(len=30)                                                  :: infile, outfile, date_time
       character(len=30)                                                  :: arg1, arg2, arg3, arg4, arg5
       character(len=30)                                                  :: long_ind1, lat_ind1




  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!READ INPUT DATA FROM COMMAND LINE !!!!!!!!!!!!!!!!!!!!!!!!!!!

       N = iargc()
       if(N.lt.1) then
         write(*,'(a)') "program arg1 arg2 arg3 arg4"
         write(*,'(a)') "arg1 = grid longitude"
         write(*,'(a)') "arg2 = grid latitude"
         write(*,'(a)') "arg3 = long index"
         write(*,'(a)') "arg4 = lat index"
     

       else
         call getarg(1,arg1)   !lon of grid cell
         call getarg(2,arg2)   !lat of grid cell
         call getarg(3,arg3)   !lon index
         call getarg(4,arg4)   !lat index 
             
         read(arg1,*) reallong
         read(arg2,*) lat
         read(arg3,*) long_ind
         read(arg4,*) lat_ind

       endif 

!!!!!!!!!!!!!!!!!!!!!!!!!!! MAINTAIN 3 DIGITS IN LONG AND LAT GRID INDEX WHILE READING AND WRITING MET FILE !!!!!!!!!!!!!!
!!!!!!!!!! e.g. 31_42.MET to 031_042.bin !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       infile = trim(adjustl(arg3))//'_'//trim(adjustl(arg4))//'_6h.MET'  ! creates input file name to match CRUNCEP original file name
  
       if(long_ind.lt.10) then
         long_ind1 = '00'//trim(adjustl(arg3))
       else if(long_ind.lt.100) then
         long_ind1 = '0'//trim(adjustl(arg3))      
       else
         long_ind1 = trim(adjustl(arg3))
       end if 


       if(lat_ind.lt.10) then
         lat_ind1 = '00'//trim(adjustl(arg4))
       else if(lat_ind.lt.100) then
         lat_ind1 = '0'//trim(adjustl(arg4))      
       else
         lat_ind1 = trim(adjustl(arg4))
       end if 
 
       outfile = trim(adjustl(long_ind1))//'_'//trim(adjustl(lat_ind1))//'.bin'


!!!!!!!!!!!!    READ 6-HOUR MET FILE AND GENERATE A HALF-HOURLY BINARY FILE !!!!!!!!!!!!!!!!

       open(unit=11, file=infile, status='old')
       open(unit=40, file=outfile, form = 'unformatted',access='sequential', &
            position='append', status='replace')   

!!!!! CONVERT LAT TO RADIAN !!!

       reallat = lat * (pi / 180.0) 

!!!!!!!!!!!!!!!!!!! Reading variables !!!!!!!!!!!!!!

       read(11,*) (var(1,j),j=1,size_col)  !read first line of the file
       read(11,*) (var(2,j),j=1,size_col)
       read(11,*) (var(3,j),j=1,size_col)
       read(11,*) (var(4,j),j=1,size_col)
       read(11,*) (var(5,j),j=1,size_col)
       read(11,*) (var(6,j),j=1,size_col)
       read(11,*) (var(7,j),j=1,size_col)
       read(11,*) (var(8,j),j=1,size_col)
       read(11,*) (var(9,j),j=1,size_col)
       
       year = start_year - 1   

       do 100 i = 10,size_row,4
       day = mod(((i-8)/4), 365) 
         if(day.eq.0) year = year + 1
         day = day + 1    !day start from one


            if (i.le.(size_row-3)) then ! i represents row of the 6-hour MET file, go through before last 3 lines of the file
               read(11,*) (var(10,j),j=1,size_col)
               read(11,*) (var(11,j),j=1,size_col)
               read(11,*) (var(12,j),j=1,size_col)
               read(11,*) (var(13,j),j=1,size_col)
               
               par(1,:) = var(1,:)
               par(2,:) = var(2,:)
               par(3,:) = var(3,:)
               par(4,:) = var(4,:)
               par(5,:) = var(5,:)

               call disaggregate(par, reallat, reallong, day,&
                     SW_interpolated_num_48_1, LW_interpolated_num_48_1,PCP_interpolated_num_48_1,T_interpolated_num_48_1,&
                     H_interpolated_num_48_1,W_interpolated_num_48_1,P_interpolated_num_48_1,lcltmind)

               par(1,:) = var(5,:)
               par(2,:) = var(6,:)
               par(3,:) = var(7,:)
               par(4,:) = var(8,:)
               par(5,:) = var(9,:)

               call disaggregate(par, reallat, reallong, day+1,&
                     SW_interpolated_num_48_2, LW_interpolated_num_48_2,PCP_interpolated_num_48_2,T_interpolated_num_48_2,&
                     H_interpolated_num_48_2,W_interpolated_num_48_2,P_interpolated_num_48_2,lcltmind)


               par(1,:) = var(9,:)
               par(2,:) = var(10,:)
               par(3,:) = var(11,:)
               par(4,:) = var(12,:)
               par(5,:) = var(13,:)

               call disaggregate(par, reallat, reallong, day+2,&
                     SW_interpolated_num_48_3, LW_interpolated_num_48_3,PCP_interpolated_num_48_3,T_interpolated_num_48_3,&
                     H_interpolated_num_48_3,W_interpolated_num_48_3,P_interpolated_num_48_3,lcltmind)

               SW_interpolated_num  = [SW_interpolated_num_48_1,SW_interpolated_num_48_2,SW_interpolated_num_48_3]
               LW_interpolated_num  = [LW_interpolated_num_48_1,LW_interpolated_num_48_2,LW_interpolated_num_48_3]               
               PCP_interpolated_num = [PCP_interpolated_num_48_1,PCP_interpolated_num_48_2,PCP_interpolated_num_48_3]
               T_interpolated_num   = [T_interpolated_num_48_1,T_interpolated_num_48_2,T_interpolated_num_48_3]
               H_interpolated_num   = [H_interpolated_num_48_1,H_interpolated_num_48_2,H_interpolated_num_48_3]
               W_interpolated_num   = [W_interpolated_num_48_1,W_interpolated_num_48_2,W_interpolated_num_48_3]
               P_interpolated_num   = [P_interpolated_num_48_1,P_interpolated_num_48_2,P_interpolated_num_48_3]

               var(1,:) = var(5,:)
               var(2,:) = var(6,:)
               var(3,:) = var(7,:)
               var(4,:) = var(8,:)
               var(5,:) = var(9,:)
               var(6,:) = var(10,:)
               var(7,:) = var(11,:)
               var(8,:) = var(12,:)
               var(9,:) = var(13,:)


            else ! last 3 lines of the files 


               read(11,*) (var(10,j),j=1,size_col)
               read(11,*) (var(11,j),j=1,size_col)
               read(11,*) (var(12,j),j=1,size_col)
               var(13,:) = var(6,:) ! 6 or 1

               par(1,:) = var(1,:)
               par(2,:) = var(2,:)
               par(3,:) = var(3,:)
               par(4,:) = var(4,:)
               par(5,:) = var(5,:)

               call disaggregate(par, reallat, reallong, day,&
                     SW_interpolated_num_48_1, LW_interpolated_num_48_1,PCP_interpolated_num_48_1,T_interpolated_num_48_1,&
                     H_interpolated_num_48_1,W_interpolated_num_48_1,P_interpolated_num_48_1,lcltmind)


               par(1,:) = var(5,:)
               par(2,:) = var(6,:)
               par(3,:) = var(7,:)
               par(4,:) = var(8,:)
               par(5,:) = var(9,:)

               call disaggregate(par, reallat, reallong, day+1,&
                     SW_interpolated_num_48_2, LW_interpolated_num_48_2,PCP_interpolated_num_48_2,T_interpolated_num_48_2,&
                     H_interpolated_num_48_2,W_interpolated_num_48_2,P_interpolated_num_48_2,lcltmind)

               par(1,:) = var(9,:)
               par(2,:) = var(10,:)
               par(3,:) = var(11,:)
               par(4,:) = var(12,:)
               par(5,:) = var(13,:)
               
               call disaggregate(par, reallat, reallong, day+2,&
                     SW_interpolated_num_48_3, LW_interpolated_num_48_3,PCP_interpolated_num_48_3,T_interpolated_num_48_3,&
                     H_interpolated_num_48_3,W_interpolated_num_48_3,P_interpolated_num_48_3,lcltmind)

               SW_interpolated_num  = [SW_interpolated_num_48_1,SW_interpolated_num_48_2,SW_interpolated_num_48_3]
               LW_interpolated_num  = [LW_interpolated_num_48_1,LW_interpolated_num_48_2,LW_interpolated_num_48_3]               
               PCP_interpolated_num = [PCP_interpolated_num_48_1,PCP_interpolated_num_48_2,PCP_interpolated_num_48_3]
               T_interpolated_num   = [T_interpolated_num_48_1,T_interpolated_num_48_2,T_interpolated_num_48_3]
               H_interpolated_num   = [H_interpolated_num_48_1,H_interpolated_num_48_2,H_interpolated_num_48_3]
               W_interpolated_num   = [W_interpolated_num_48_1,W_interpolated_num_48_2,W_interpolated_num_48_3]
               P_interpolated_num   = [P_interpolated_num_48_1,P_interpolated_num_48_2,P_interpolated_num_48_3]

            end if 

!!!!!!!!!!!!!!!!!!!!!!!! SHIFTING VARIABLES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                

             do 140 count = 1, quantity
              shift = count + (48.0 - lcltmind - 6.0)
              if(shift.gt.quantity)shift = shift - quantity
                LW_shifted_num(count) = LW_interpolated_num(shift)
                PCP_shifted_num(count) = PCP_interpolated_num(shift)
                T_shifted_num(count) = T_interpolated_num(shift)
                H_shifted_num(count) = H_interpolated_num(shift)
                W_shifted_num(count) = W_interpolated_num(shift)
                P_shifted_num(count) = P_interpolated_num(shift)

140          continue 


!!!!!!!!!!!!!!!!!!!!!!!!! GENERATING HOUR AND MINUTE COLUMN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	       do y = 1,144                
	         hour = (y-1)/2
                 hour  = mod(hour,24)
	         z = mod(y,2)
	         if(z.eq.1) then
	           minute = 0
	         else 
		   minute = 30
	         endif 
                   hour1(y) = hour 
                   minute1(y) = minute
               end do

!!!!!!!!!!!!!!!!!!!!!!!!! WRITING A MET FILE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

              do k = 49, 96  !smooth data, first 48 and last 48 data has jumps due to shifting 
                    write(40) hour1(k), minute1(k), day, year, SW_interpolated_num(k),LW_shifted_num(k), &
                    PCP_shifted_num(k), T_shifted_num(k),H_shifted_num(k),W_shifted_num(k),P_shifted_num(k)
              end do 
              
                  

100    continue !day loop 

!!!!!!!!!!!!!!! We are short of two days data at the end of the file, so append last two days data !!!!!!!!!!!!!!!!!! 

              day1(1:48) = 363  ! as we don't have last 3 days so we construct day arrays
              day1(49:96) = 364
              day1(97:144) = 365
 
              do k = 49, 144
                    write(40) hour1(k), minute1(k), day1(k), year, SW_interpolated_num(k),LW_shifted_num(k),  & 
                    PCP_shifted_num(k), T_shifted_num(k),H_shifted_num(k),W_shifted_num(k),P_shifted_num(k)
              end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 

       close(unit=11)  !infile
       close(unit=40)  !outfile
 
end program runmet_ctem






