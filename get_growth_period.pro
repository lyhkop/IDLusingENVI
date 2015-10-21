;
;Name:
;    
;Purpos:
;       提取曲线增长的一部分
;                
;Input:
;    
;Output:
;    
;Exmple:
;       
;Theory:
;       
;Author:
;       lyh/~_~/@2015.10.16
;
;history: 
;

Pro get_growth_period, input, output
  
  compile_opt idl2
  
  ON_ERROR,2   ;return caller function
  
  n_dimensions = size(input, /N_DIMENSIONS)
  elements = n_elements(input)
  if (n_dimensions ne 1) then begin 
    message, 'input must be on dimensions array!'
  endif 
  if (elements lt 3) then begin
    message, 'input must have at least three elements!'
  endif
  
  ;this part must be modified! 
  ;because this part only work with one type curve!
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  min_value = min(input[1:-2], min_subscript, subscript_max=max_subscript)
  min_subscript = min_subscript-1
  max_subscript = max_subscript+1
  
  input_subscript = indgen(elements)
  X = (min_subscript le max_subscript) ? input_subscript[(min_subscript+1):(max_subscript+1)] : $
                                         input_subscript[(max_subscript+1):(min_subscript+1)] 
  Y = (min_subscript le max_subscript) ? input[(min_subscript+1):(max_subscript+1)] : $
                                         input[(max_subscript+1):(min_subscript+1)]
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;return output
  if (n_elements(X) le 3) then begin
    output = make_array(2, 4, type=size(input, /TYPE))
  endif else begin
    output = transpose([[X], [Y]])
  endelse
  
  
End