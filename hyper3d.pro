FUNCTION HYPER3D, M, ns, nl, nb

  ;ns - 列数        nl - 行数         nb - 波段数
  ;输入二维图像，输出三维
  COMPILE_OPT idl2
  IF SIZE(M, /n_dimensions) NE 2 AND nb NE 1 THEN BEGIN
    mytemp = DIALOG_MESSAGE('Input image must be 2D')
  ENDIF ELSE BEGIN
    M_temp = FLTARR(ns, nl, nb)
    FOR i = 0, nb-1 DO BEGIN
      M_temp[*,*,i] = TRANSPOSE(REFORM(TRANSPOSE(M[*,i]), nl, ns))
    ENDFOR
    RETURN, TEMPORARY(M_temp)
  ENDELSE
  
END