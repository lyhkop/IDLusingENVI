;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释[~_~];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 函数名：
; Shift_Mean
; 调用方式：
; Shift_Mean(data, flag)
;
; 目的：
; 将二维数组中的列子集先按最大值/最小值位置进行匹配、平移，然后对平移后的列子集求平均值
; 如: A=[1, 2, 3]
;       [3, 5, 2]
;       [2, 2, 4]
;       [1, 2, 4] 
; 最大值位置匹配后（第一列为基准， 第二列最大值位置与第一列相同不平移， 第三列向左平移一个元素）:
;     A=[1, 2, 2]， 求平均值： [1.67] 
;       [3, 5, 4]            [4.00]
;       [2, 2, 4]            [2.67]
;       [1  2, 0]            [1.00]
;     
; 输入参数：
; data 二维数组（一维数组直接返回）
; flag 匹配方式（flag=0 最大值匹配， flag=1 最小值匹配）
;
; 返回值：
; data列子集匹配平移后的平均值
;
; 备注：
;;;;;;;;;;;;;;;;;;;;;;;;lyh的注释\~_~\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Function shift_mean, data, flag
;小于二维直接返回

;重要变换!
;data = transpose(data, [2, 1])
  if size(data, /N_DIMENSIONS) lt 2 then begin
    return, data
  endif

;返回data各维维度，dimensions[0]为列数，dimensions[1]为行数
  dimensions = size(data, /dimensions)
;存储data中各列子集中最大值的位置
  max_pos = intarr(dimensions[0])
  min_pos = intarr(dimensions[0])
  for i=0l, dimensions[0]-1 do begin
    max_value = max(data[i, *], max_subscript, SUBSCRIPT_MIN=min_subscript)
    max_pos[i] = max_subscript
    min_pos[i] = min_subscript
  endfor

;确定data中各列子集的平移距离
  switch (flag) of
    0: begin
    
      distances = intarr(dimensions[0])
      for i=0l, dimensions[0]-1 do begin
        distances[i] = max_pos[i] - max_pos[0]
      endfor
      
      break
    end
    else: begin
      distances = intarr(dimensions[0])
      for i=0l, dimensions[0]-1 do begin
        distances[i] = min_pos[i] - min_pos[0]
      endfor
    end
  endswitch



;对各列进行平移
  for i=0l, dimensions[0]-1 do begin
    data[i, *] = MyShift(data[i, *], distances[i])
  endfor
;对平移后的列子集求平均值
  ;r_data = mean(data, dimension=1) 
  r_data = MyMean(data)
  return, r_data 
End

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
  M_temp = M
  FOR i = 0, nb-1 DO BEGIN
    M_temp[*,*,i] = TRANSPOSE(M[*,*,i])
  ENDFOR
  RETURN, REFORM(TEMPORARY(M_temp), dims[0]*dims[1], nb)
  
END

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

Pro get_mean_spectrum

  compile_opt idl2
  
  envi, /restore_base_save_files
  envi_batch_init
  
  input_filenames = ENVI_PICKFILE(TITLE='请输入多年的时间序列数据', /MULTIPLE_FILES)
  if n_elements(input_filenames) eq 0 then return  
  
  output_filename = envi_pickfile(title='输出文件名' ,/output)
  if (output_filename eq '') then return
  
  wbase = widget_auto_base(title='选择曲线匹配方式')
    wtoggle = widget_toggle(wbase, list=['最大值匹配', '最小值匹配'], $
    uvalue='mtoggle', /auto)
  wresult = auto_wid_mng(wbase)
  if (wresult.accept eq 0) then return
  ;0为最大值匹配, 1为最小值匹配
  pipei_flag = wresult.mtoggle
  
  ;打开输入文件
  input_file_count = n_elements(input_filenames)
  input_fileids = intarr(input_file_count)
  for i=0l, input_file_count-1 do begin
    envi_open_file, input_filenames[i], r_fid=r_fid
    if r_fid eq -1 then return
    input_fileids[i] = r_fid
  endfor
  
  
  envi_file_query, input_fileids[0], dims=dims, $
  ns=ns, $
  nl=nl, $
  nb=nb, $
  data_type=data_type, $
  interleave=interleave
  
  ;打开输出文件
  openw, output_file_unit, output_filename, /get_lun
  
  ;分块处理数据
  tile_nums = 10
  tile_lines = [nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl/tile_nums, $
                nl/tile_nums, nl-nl/tile_nums*9]
  
  ;循环处理Tiles的内容
  for i=0l, tile_nums-1 do begin
    
    input_data = make_array(ns, tile_lines[i], $
    nb, input_file_count, type=data_type)
    
    for j=0l, input_file_count-1 do begin
      for k=0l, nb-1 do begin
        input_data[*, *, k, j] = envi_get_data(fid=input_fileids[j], $
        dims=[dims[0], $
              dims[1], $
              dims[2], $
              i*nl/tile_nums, $
              i*nl/tile_nums+tile_lines[i]-1], $
              pos=k)
      endfor
    endfor
    
    ;将输入的3维块数据转换为2维float型数据
    input_data2d = make_array(ns*tile_lines[i], nb, input_file_count)
    for l=0, input_file_count-1 do begin
      input_data2d[*, *, l] = HYPER2D(input_data[*, *, *, l])
    endfor
    
    ;创建输出2维数据
    output_data = make_array(ns*tile_lines[i], nb)
    
    ;开始计算平均数据
    for m=0l, ns*tile_lines[i]-1 do begin
      output_data[m, *] = shift_mean(input_data2d[m, *, *], pipei_flag)
    endfor
    
    output_data = HYPER3D(output_data, ns, tile_lines[i], nb)
    
    ;将数据存储方式转换为BIL，并写入输出文件中
    output_data = transpose(temporary(output_data), [0, 2, 1])
    writeu, output_file_unit, output_data 
  
    print, '第', i, '块'
    
  endfor   
  
  ;关闭文件  
  free_lun, output_file_unit
  
  map_info = envi_get_map_info(fid=input_fileids[0])
  envi_setup_head, fname=output_filename, ns=ns, nl=nl, nb=nb, map_info=map_info, $ 
  data_type=4, offset=0, interleave=1, $
  descrip='Test routine output', /write, /open
  
  envi_batch_exit

End