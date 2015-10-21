;
;Name:
;    do_logistic_fit, input, output, A=a
;Purpos:
;    对输入数据input进行logistic曲线拟合，logistic函数形式见Theory部分
;Input:
;    输入为两列数据[X, Y]
;Output:
;    
;Exmple:
;
;Theory:
;       f(x) = c /(1 + e^ax+b)
;       
;Author:
;       lyh/~_~/@2015.10.16


Pro my_logistic_function, X, A, F, PDER

  compile_opt idl2
  
  exp_part = exp(A[0]*X+A[1])
  F = A[2] / (1 + exp_part)
  
  if n_params() ge 4 then $
    PDER = [[-A[2]*X*exp_part/(1+exp_part)^2], $
           [-A[2]*exp_part/(1+exp_part)^2],   $
           [1/(1+exp_part)]]

End

Pro do_logistic_fit, input_data, output_data, A=a
  
  compile_opt idl2
  
  X = transpose(input_data[0, *])
  Y = transpose(input_data[1, *])
  
  if (n_elements(a) eq 0) then a=[1.0, 1.0, 1.0]
  
  yfit = CURVEFIT(X, Y, weight, A, FUNCTION_NAME='my_logistic_function', /DOUBLE, YERROR=yerror, STATUS=status)
  
  output_data = [status, yerror]
  
End