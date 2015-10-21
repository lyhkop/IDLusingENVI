;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 函数名：
; MyMean
;
; 目的：
; 对二维数组的列子集进行平均值的计算，且0元素不参与运算。
; 如：
; data = [[1,2,1], [1,2,0], [1,0,0], [0,0,0]]
; 第一列：(1+2+1)/3, 第二列：(1+2)/2, 第三列：(1)/1, 第四列：0
; 
; 输入参数：
; data为一个二维数组
;
; 返回值：
; data列子集的平均值
;
; 备注：
;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释\~_~\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Function MyMean, data
;data的一个列子集含有多少个元素
  dimensions = size(data, /DIMENSIONS)
; 函数的返回值 
  r_data=fltarr(dimensions[1])
  for i=0l, dimensions[1]-1 do begin
    subscript = where(data[*, i] eq 0, count)
    if count eq dimensions[0] then begin
      r_data[i] = 0
    endif else begin
      r_data[i] = total(data[*, i])/(dimensions[0]-count)
    endelse
  endfor
  ;r_data = TRANSPOSE(r_data)
  return, r_data
End

