FUNCTION HYPER2D, M

  ;输入三维图像，输出二维
  COMPILE_OPT idl2
  dims = SIZE(M, /dimensions)
   
  IF SIZE(M, /n_dimensions) NE 3 AND SIZE(M, /n_dimensions) NE 2 THEN BEGIN
    mytemp = DIALOG_MESSAGE('Input image must be 3D')
  ENDIF ELSE IF  SIZE(M, /n_dimensions) EQ 2 THEN BEGIN
    nb = 1
  ENDIF ELSE BEGIN
    nb = dims[2]
  ENDELSE
  
  ;转置矩阵
  ;M_temp = M
  FOR i = 0, nb-1 DO BEGIN
    ;M_temp[*,*,i] = TRANSPOSE(M[*,*,i])
    M[*,*,i] = TRANSPOSE(M[*,*,i])
  ENDFOR
  ;RETURN, REFORM(TEMPORARY(M_temp), dims[0]*dims[1], nb)
  RETURN, REFORM(TEMPORARY(M), dims[0]*dims[1], nb)
  
END