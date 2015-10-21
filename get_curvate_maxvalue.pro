Function logistic_curvate_maxvalue, a, b, c

  compile_opt idl2
  
  P = c^4/27. + c^2/b^2
  q = 2*c^6/729. - c^4/(18.*b^2)
  
  tmp = -q/2. * ((P/3.)^3)^(0.5)
  theta = (1./3.)*acos(tmp)
  
  y1 = c/2. - (5*c^2/36 + 2*(P/3)^(1/2)*cos(theta+4*(!Pi)/3))^(1/2.)
  y2 = c/2. + (5*c^2/36 + 2*(P/3)^(1/2)*cos(theta+4*(!Pi)/3))^(1/2.)
  
  ;
  ;y = c / (1 + e^(a*x + b))
  ;
  ;x = (ln(c/y -1) - b) / a
  ;
  x1 = (alog(c/y1 - 1) - b)/a
  x2 = (alog(c/y2 - 1) - b)/a
  
  return, [x1, x2]
  
End

Pro get_curvate_maxvalue, input, output, other_params

  compile_opt idl2
  
    min_value = min(input, max=max_value)
     
  if (min_value eq max_value) then begin
    output=input[0:3]
  endif else begin
        
    get_growth_period, input, growth_period
    start_t = growth_period[0,0]
    end_t   = growth_period[0, -1]
    
    A = [1.0,1.0,1.0]
    do_logistic_fit, growth_period, A=A, logistic_out
    
    ;logistic_out[status, yerror]
    status = logistic_out[0]
    yerror = logistic_out[1] 
      
    curvate_maxvalue_pos = logistic_curvate_maxvalue(A[0], A[1], A[2])
    
    output = [curvate_maxvalue_pos[0]+1, curvate_maxvalue_pos[1]+1, $
              status, yerror]
        
  endelse
  
End