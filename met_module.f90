module met_module

implicit none 


public 	:: disaggregate 
public	:: linear_dist
public 	:: rad_diurnal_dist
public	:: uniform_dist
public	:: precip_dist

contains 

subroutine disaggregate(variable, reallat, reallong, day, &					   !input			
!---------------------------------------------------------------------------------------------------
 	SW_interpolated_num_48, LW_interpolated_num_48,PCP_interpolated_num_48,&
	T_interpolated_num_48,H_interpolated_num_48, W_interpolated_num_48,P_interpolated_num_48,lcltmind ) ! output)

	implicit none 

	integer, parameter							:: size_col = 7   
	real, dimension(5)							:: T,H,W,P
	real, dimension(5, size_col) 						:: variable
	integer 								:: i, j
	integer									:: day, lcltmind
	real									:: data1
	real									:: data2, reallat, reallong
	
	real, dimension(4)							:: SW, SW4   		
	real, dimension(4)							:: LW, PCP  
	real, dimension(48)							:: SW_interpolated_num_48 
	real, dimension(:), allocatable 					:: LW_interpolated_num_48
	real, dimension(:), allocatable 					:: PCP_interpolated_num_48
	real, dimension(:), allocatable 					:: T_interpolated_num_48
	real, dimension(:), allocatable 					:: H_interpolated_num_48
	real, dimension(:), allocatable 					:: W_interpolated_num_48
	real, dimension(:), allocatable 					:: P_interpolated_num_48

	real, dimension(12)							:: LW_interpolated_num_1
	real, dimension(12)							:: LW_interpolated_num_2
	real, dimension(12)							:: LW_interpolated_num_3
	real, dimension(12)							:: LW_interpolated_num_4

	real, dimension(12)							:: PCP_interpolated_num_1
	real, dimension(12)							:: PCP_interpolated_num_2
	real, dimension(12)							:: PCP_interpolated_num_3
	real, dimension(12)							:: PCP_interpolated_num_4

	real, dimension(12)							:: T_interpolated_num_1
	real, dimension(12)							:: T_interpolated_num_2
	real, dimension(12)							:: T_interpolated_num_3
	real, dimension(12)							:: T_interpolated_num_4

	real, dimension(12)							:: H_interpolated_num_1
	real, dimension(12)							:: H_interpolated_num_2
	real, dimension(12)							:: H_interpolated_num_3
	real, dimension(12)							:: H_interpolated_num_4

	real, dimension(12)							:: W_interpolated_num_1
	real, dimension(12)							:: W_interpolated_num_2
	real, dimension(12)							:: W_interpolated_num_3
	real, dimension(12)							:: W_interpolated_num_4

	real, dimension(12)							:: P_interpolated_num_1
	real, dimension(12)							:: P_interpolated_num_2
	real, dimension(12)							:: P_interpolated_num_3
	real, dimension(12)							:: P_interpolated_num_4


!!!!!!!!!!!!!!!!!! SHORT WAVE RADIATION !!!!!!!!!!!!!!!

	SW(1) = variable(1,1)
	SW(2) = variable(2,1)
	SW(3) = variable(3,1)
	SW(4) = variable(4,1)

	SW4 = [SW(1)/21600.00, SW(2)/21600.00, SW(3)/21600.00, SW(4)/21600.00]
	CALL rad_diurnal_dist(SW4, reallat, day, reallong, SW_interpolated_num_48,lcltmind)

!!!!!!!!!!!!!!!!! LONG-WAVE RADIATION !!!!!!!!!!!!!!!!!!!!!!!!!				

	LW(1) = variable(1,2)
	LW(2) = variable(2,2)
	LW(3) = variable(3,2)
	LW(4) = variable(4,2)

	call uniform_dist(LW(1),LW_interpolated_num_1)
	call uniform_dist(LW(2),LW_interpolated_num_2)
	call uniform_dist(LW(3),LW_interpolated_num_3)
	call uniform_dist(LW(4),LW_interpolated_num_4)
	LW_interpolated_num_48 = [LW_interpolated_num_1,LW_interpolated_num_2,LW_interpolated_num_3,LW_interpolated_num_4]


!!!!!!!!!!!!!!!!!!!!!!!!! PRECIPITTION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!				

	PCP(1) = variable(1,3)
	PCP(2) = variable(2,3)
	PCP(3) = variable(3,3)
	PCP(4) = variable(4,3)

	call precip_dist(PCP(1),PCP_interpolated_num_1)
	call precip_dist(PCP(2),PCP_interpolated_num_2)
	call precip_dist(PCP(3),PCP_interpolated_num_3)
	call precip_dist(PCP(4),PCP_interpolated_num_4)
	PCP_interpolated_num_48 = [PCP_interpolated_num_1,PCP_interpolated_num_2,PCP_interpolated_num_3,PCP_interpolated_num_4]


!!!!!!!!!!!!!!!!!!!! TEMPRATURE !!!!!!!!!!!!!!!!!!!!!!!

	T(1) = variable(1,4)
	T(2) = variable(2,4)
	T(3) = variable(3,4)
	T(4) = variable(4,4)
	T(5) = variable(5,4)

	call linear_dist(T(1)-273.16,T(2)-273.16,T_interpolated_num_1)
	call linear_dist(T(2)-273.16,T(3)-273.16,T_interpolated_num_2)
	call linear_dist(T(3)-273.16,T(4)-273.16,T_interpolated_num_3)
	call linear_dist(T(4)-273.16,T(5)-273.16,T_interpolated_num_4)
	T_interpolated_num_48 = [T_interpolated_num_1,T_interpolated_num_2,T_interpolated_num_3,T_interpolated_num_4]


!!!!!!!!!!!!!!!!!! HUMIDITY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	H(1) = variable(1,5)
	H(2) = variable(2,5)
	H(3) = variable(3,5)
	H(4) = variable(4,5)
	H(5) = variable(5,5)

	call linear_dist(H(1),H(2),H_interpolated_num_1)
	call linear_dist(H(2),H(3),H_interpolated_num_2)
	call linear_dist(H(3),H(4),H_interpolated_num_3)
	call linear_dist(H(4),H(5),H_interpolated_num_4)
	H_interpolated_num_48 = [H_interpolated_num_1,H_interpolated_num_2,H_interpolated_num_3,H_interpolated_num_4]


!!!!!!!!!!!!!!!!!! WIND !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	W(1) = variable(1,6)
	W(2) = variable(2,6)
	W(3) = variable(3,6)
	W(4) = variable(4,6)
	W(5) = variable(5,6)

	call linear_dist(W(1),W(2),W_interpolated_num_1)
	call linear_dist(W(2),W(3),W_interpolated_num_2)
	call linear_dist(W(3),W(4),W_interpolated_num_3)
	call linear_dist(W(4),W(5),W_interpolated_num_4)
	W_interpolated_num_48 = [W_interpolated_num_1,W_interpolated_num_2,W_interpolated_num_3,W_interpolated_num_4]


!!!!!!!!!!!!!!!!!! PRESSURE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	P(1) = variable(1,7)
	P(2) = variable(2,7)
	P(3) = variable(3,7)
	P(4) = variable(4,7)
	P(5) = variable(5,7)

	call linear_dist(P(1),P(2),P_interpolated_num_1)
	call linear_dist(P(2),P(3),P_interpolated_num_2)
	call linear_dist(P(3),P(4),P_interpolated_num_3)
	call linear_dist(P(4),P(5),P_interpolated_num_4)
	P_interpolated_num_48 = [P_interpolated_num_1,P_interpolated_num_2,P_interpolated_num_3,P_interpolated_num_4]


	return 

	end subroutine disaggregate


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! LINEAR INTETPOLATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine linear_dist(first_num, second_num, &          	!input 
!--------------------------------------------------------------
				y)					!output
	implicit none
	real,intent(in)						:: first_num
	real,intent(in)						:: second_num
	integer							:: z
	real,dimension(12)					:: y
	real,parameter, dimension(12) :: x = [ 0., 1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11. ] !avoid overlapping of data when start with 0
 
	do 50 z = 1,12  
	    y(z) = first_num + ((second_num - first_num) * (x(z) / 12))
 50 continue
	return
	end subroutine linear_dist




!!!!!!!!!!!!!!!!!!!!!! SHORT-WAVE DISTRUBUTION !!!!!!!!!!!!!!!!!


	subroutine rad_diurnal_dist(values,rad_lat,day,longt, &    	! input
	!----------------------------------------------------------
		        zen_s_fixed,lcltmind)                               	! output

	integer					:: sum1
	integer					:: daylight_count
	integer					:: lcltmind

	integer					:: day_length
	integer					:: raw_size
	integer					:: quantity, day, count, time, K
	integer, parameter			:: day_minutes = 1440
	integer, parameter			:: time_step = 30
	integer, parameter			:: resolution = time_step
	integer, parameter			:: final_quantity = day_minutes / resolution
	integer, parameter			:: max_size = day_minutes / time_step
	integer					:: daylight_indices(max_size)
	integer, parameter			:: num_of_values = 4

	real					:: interval(max_size)
	real					:: zenith_cos(max_size)
	real					:: zenith_values(max_size)
	real					:: mean_s	
	real					:: rad_lat, lat, longt
	real, dimension(4)			:: values 
	real, dimension(final_quantity)		:: zen_s_fixed
	real, dimension(1)			:: zenith_noon
	real, parameter				:: pi = 3.14159265
	real, parameter				:: noon(1) = 720

!    FINDS THE MEAN OF THE INPUT VALUES

		      sum1 = 0.0
		      do K = 1, num_of_values
			sum1 = sum1 + values(K)
		      enddo
		      mean_s = sum1/num_of_values

		     
			count = 1
		      do 110 time = 0, (day_minutes - time_step), time_step
			interval(count) = time
			count = count + 1
		110   continue

		      	quantity = max_size
		      	call get_zenith(interval , quantity, day, rad_lat, zenith_values)

			call get_zenith(noon, 1, day, rad_lat, zenith_noon)

		!     FIND THE DAY LENGTH

		      quantity = max_size
		      daylight_count = 0
		      do 150 count = 1, quantity
			zenith_cos(count) = cos(zenith_values(count))
			if(zenith_cos(count).ge.0.0)then
			  daylight_count = daylight_count + 1
			  daylight_indices(count) = count
			else
			  daylight_indices(count)=-1
			endif
		150   continue
		      day_length = daylight_count * time_step

		!     FIND DIURNAL DISTRIBUTION OF SW RADIATION AT SPECIFIED TIME STEP

		      raw_size = daylight_count
		      call get_zen_s(zenith_values, quantity, raw_size, daylight_indices, &
				     zenith_noon, mean_s, longt,time_step, zen_s_fixed,lcltmind)
	return 
	end subroutine rad_diurnal_dist


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	subroutine get_zenith(times, quantity, day, rad_lat, &     	!input
!--------------------------------------------------------------
				out_zenith)				!output

	implicit none 

	integer					:: quantity, day, count
	real					:: times(quantity)
	integer, dimension(4)			:: N
	real					:: rad_lat, rad_hour_angle
	real					:: deg_hour_angle, psi, dec
	real, dimension(4)			:: A, B
	real, dimension(*)			:: out_zenith
	integer, parameter 			:: days_in_year = 365
	integer, parameter 			:: minutes_in_hour = 60
	real, parameter 			:: pi = 3.14159265


	      A =(/0.006918, -0.399912, -0.006758, -0.002697/)
	      B = (/       0.0,  0.070257,  0.000907,  0.001480/)
	      N =  (/       0,         1,         2,         3/)

	      psi = (2 * pi * (day - 1)) / days_in_year
	      dec = sum((A * cos(N * psi)) + (B * sin(N * psi)))

	!     FIND THE HOUR ANGLE, CONVERT IT TO RADIANS THEN FIND THE ZENITH
	!     ANGLE(S).

	      do 100 count = 1, quantity
		deg_hour_angle = 15 * (12 - (times(count) / minutes_in_hour))
		rad_hour_angle = (deg_hour_angle/ 360) * 2 * pi
		out_zenith(count) = acos((sin(rad_lat) * sin(dec)) + &
	 	(cos(rad_lat) * cos(dec) * cos(rad_hour_angle)))
	100   continue
	return 
	end subroutine get_zenith
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine get_zen_s(zenith_values,quantity,raw_size,daylight_indices, &
				zenith_noon, mean_s, longt, time_step, &		!input 
	!------------------------------------------------------------------------------
					zen_s,lcltmind)						! output 


		implicit none 

		real, dimension(:), allocatable  :: s_raw

		integer					:: raw_size
		integer					:: count
		integer					:: daylight_indices(*)
		integer					:: quantity
		integer					:: test
		integer					:: local_time
		integer					:: K
		integer					:: lcltmind
		integer					:: shift
		integer					:: time_step
		integer, parameter			:: zero = 0

		real					:: zenith_values(*)
		real					:: zenith_noon(1)
		real					:: mean_s
		real					:: zen_s(*)
		real					:: sum1
		real					:: correction
		real					:: mean_s_raw
		real					:: longt
		real, parameter				:: pi = 3.14159265
	

	      allocate(s_raw(quantity))

	      sum1 = 0.0
	      do 120 count = 1, quantity
		test = daylight_indices(count)

	!      CHECK WITH DAYLIGHT_INDICES TO SEE IF IT THERE IS DAYLIGHT AT
	!       THE TIME, AND IF THERE IS, THEN FIND THE VALUE FOR S, SETTING
	!       IT TO ZERO OTHERWISE.

		if (test.gt.zero) then
		  s_raw(count) = ((mean_s * pi) / 2) * cos(((zenith_values(test)  &
		        - zenith_noon(1)) / ((pi / 2) - zenith_noon(1))) * (pi / 2))
		else
		  s_raw(count) = zero
		end if
		sum1 = sum1 + s_raw(count)

	120   continue 
	      mean_s_raw = sum1/quantity

	      if (mean_s_raw.ne.zero) then
		correction = mean_s / mean_s_raw
	      else
		correction = zero
	      end if 

	!     FIND WHAT'S THE LOCAL TIME WHEN GMT EQUALS MIDNIGHT. ASSUMING THAT
	!     EVERY 15 DEGREE LONGITUDE EQUALS 1 HOUR.

	      local_time = nint(longt*4.0) ! LOCAL TIME IN MINUTES


	!     FIND WHICH TIME SLOT INDEX DOES THIS TIME FITS IN

	      lcltmind = 0 ! LOCAL TIME INDEX
	      do 130 K = 0, quantity-1, 1
		if( local_time.ge.K*time_step.and.       &  
		   local_time.lt.(K+1)*time_step )then
		  lcltmind = K+1 ! LOCAL TIME INDEX
		end if 
	130   continue 

	      if(lcltmind.eq.0)then
		write(6,*)'LOCAL TIME INDEX ZERO'
	!        PAUSE 
	      end if

	!     APPLY THE CORRECTION AND SHIFT SO THAT NUMBERS START AT GMT MIDNIGHT


!              write(*,*)'lcltmind = ',lcltmind
	      do 140 count = 1, quantity
		shift = count + lcltmind
		if(shift.gt.quantity)shift = shift - quantity
!		zen_s(count) = s_raw(shift) * correction
		zen_s(count) = s_raw(count)* correction
 !               write(*,*)'count = ',count,' shift = ',shift,' zen_s = ',zen_s(count)
!		write(*,*) 'zen_s = ',zen_s(count) , 's_raw = ',s_raw(count)
	140   continue 
		
		
		

	      deallocate(s_raw)       

	return 
	end subroutine get_zen_s
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!UNIFORM DISTRIBUTION !!!!!!!!!!!!!!!

	subroutine uniform_dist(first_num, &				!input
!------------------------------------------------------------
				y)					!output
	implicit none
	real,intent(in)						:: first_num
	real,dimension(12),intent(out)				:: y
	integer							:: z

	do 51 z = 1,12
		y(z) = first_num		
51 continue
	return
	end subroutine uniform_dist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!PRECIPITATION DISTRUBUTION !!!!!!!!!!!!!!!!!
	
	subroutine precip_dist(first_num, &				!input
!---------------------------------------------------
			ppt)						!output
	implicit none
	real,intent(in)						:: first_num
	real							:: P, wet_hh,temp,sum1
	real,dimension(12),intent(out)				:: ppt
	real,allocatable,dimension(:)				:: b
	real,dimension(12)					:: random

	integer							:: i,k,t
	integer							:: wet 
	integer,dimension(12)					:: sort_ind   	


	P = first_num !*6*60*60					! convert mm/s to mm/6hr
	P = max(P,0.0)

	if (P.gt.0.0) then
	wet_hh = 2.6*log(6.93*P)
	wet_hh = max(1.0,min(wet_hh,12.0))
	wet = nint(wet_hh)
	else
	wet_hh = 0.0
	wet = 0
	end if 

        do k = 1,12
          random(k) = rand() 
	  sort_ind(k) = k
 
        enddo

        do i = 1,12 
          do k = i,12
            if(random(i).lt.random(k)) then 
              temp = random(i) 
              random(i) = random(k) 
              random(k) = temp 
              t = sort_ind(i) 
              sort_ind(i) = sort_ind(k) 
              sort_ind(k) = t 
            endif 
          enddo 
        enddo 

        do k = 1,12 
          random(k) = 0.0
        enddo 

        sum1 = 0.0 
        do k = 1,wet
          random(sort_ind(k)) = rand()
          sum1 = sum1 + random(sort_ind(k)) 
        enddo 

        do k = 1,12
         i = mod(k,12)
         if(i.eq.0) i = 12 
         if(sum1.gt.0.0) then 
           ppt(k) = (random(i)/sum1)*P*(1./(0.5*60*60))
!           ppt(k) = (random(i)/sum1)*P*(1./(60*60))
         else 
           ppt(k)=0.0
         endif 
        enddo 

	return
	end subroutine precip_dist
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module met_module 



