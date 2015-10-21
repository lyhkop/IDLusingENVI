Pro get_dynamic_threshold, input, output, other_params
  
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
    
    ;
    ;a = A[0], b = A[1], c = A[2]
    ;
    ;y = c / (1 + e^(a*x + b))
    ;
    ;x = (ln(c/y -1) - b) / a
    ;
    e=2.71828
    threshold = other_params
    
    min_value = A[2] / (1 + e^(A[0]*start_t + A[1]))
    max_value = A[2] / (1 + e^(A[0]*end_t + A[1]))
    diff_value = max_value - min_value
    
    threshold1_y = min_value + diff_value*threshold
    threshold2_y = min_value + diff_value*(1-threshold)
    
    threshold1_x = (alog(A[2]/threshold1_y - 1) - A[1]) / A[0]
    threshold2_x = (alog(A[2]/threshold2_y - 1) - A[1]) / A[0]
    
    output = [threshold1_x, threshold2_x, status, yerror]
        
  endelse

  
End