;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; 函数名：
; MyShift
; 调用方式:
; MyShift(data, d)
; 
; 目的:
; 平移数组中的元素，平移后的边界值用0补齐
; 如：A=[1, 2, 3, 4, 5]向右平移两个元素，则返回[0, 0, 1, 2, 3]
;
; 输入参数:
; data 进行平移的数组
; d    平移距离，d为正值将向左平移反之向右平移
; 
; 函数返回值:
; r_data 平移后的数组
;
; 备注：
;+
;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Function MyShift, data, d
;
  if d eq 0 then begin
    return, data
  endif

;d大于0向左平移，d小于0向右平移
  if d gt 0 then begin
    for i=0l, n_elements(data)-1 do begin
      if i lt n_elements(data)-d then begin
        data[i] = data[i+d]
      endif else begin
        data[i] = 0
      endelse
    endfor
  endif else begin
    d=abs(d)
    for i=0l, n_elements(data)-1 do begin
      if i lt n_elements(data)-d then begin
        data[n_elements(data)-1-i] = data[n_elements(data)-1-i-d]
      endif else begin
        data[n_elements(data)-1-i] = 0
      endelse
    endfor
  endelse
  return, data
End